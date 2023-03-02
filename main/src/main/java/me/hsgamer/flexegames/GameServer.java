package me.hsgamer.flexegames;

import io.github.bloepiloepi.pvp.PvpExtension;
import lombok.Getter;
import me.hsgamer.flexegames.command.*;
import me.hsgamer.flexegames.config.LobbyConfig;
import me.hsgamer.flexegames.config.MainConfig;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.hook.LoginLogHook;
import me.hsgamer.flexegames.hook.PerInstanceInstanceViewHook;
import me.hsgamer.flexegames.hook.UpdateViewHook;
import me.hsgamer.flexegames.lobby.Lobby;
import me.hsgamer.flexegames.manager.GameArenaManager;
import me.hsgamer.flexegames.manager.GameManager;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.player.GamePlayer;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;
import me.hsgamer.flexegames.util.ProxyType;
import me.hsgamer.flexegames.util.SysOutErrRedirect;
import me.hsgamer.hscore.minestom.board.Board;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.CommandManager;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerLoginEvent;
import net.minestom.server.event.server.ServerListPingEvent;
import net.minestom.server.extras.lan.OpenToLAN;
import net.minestom.server.extras.lan.OpenToLANConfig;
import net.minestom.server.ping.ResponseData;
import org.jetbrains.annotations.ApiStatus;

import java.io.File;
import java.io.FileInputStream;
import java.util.Base64;
import java.util.Collection;
import java.util.List;

/**
 * The main class of the game server
 */
@Getter
public class GameServer {
    /**
     * The Minecraft server
     */
    private final MinecraftServer minecraftServer = MinecraftServer.init();
    /**
     * The main config
     */
    private final MainConfig mainConfig = ConfigGeneratorUtil.generate(MainConfig.class, ConfigGeneratorUtil.getConfigFile("config"), true, true);
    /**
     * The lobby config
     */
    private final LobbyConfig lobbyConfig = ConfigGeneratorUtil.generate(LobbyConfig.class, ConfigGeneratorUtil.getConfigFile("lobby"), true, true);
    /**
     * The message config
     */
    private final MessageConfig messageConfig = ConfigGeneratorUtil.generate(MessageConfig.class, ConfigGeneratorUtil.getConfigFile("messages"), true, true);
    /**
     * The game manager
     */
    private final GameManager gameManager = new GameManager(this);
    /**
     * The arena manager
     */
    private final GameArenaManager arenaManager = new GameArenaManager(this);
    /**
     * The lobby
     */
    private final Lobby lobby;

    /**
     * Create a new game server
     *
     * @param args the arguments
     */
    public GameServer(String[] args) {
        // Redirect System.out and System.err to the logger
        SysOutErrRedirect.init();

        // Redirect Throwable handler
        MinecraftServer.getExceptionManager().setExceptionHandler(throwable ->
                MinecraftServer.LOGGER.error("Throwable: " + throwable.getMessage(), throwable)
        );

        // LOBBY
        lobby = new Lobby(this);
        MinecraftServer.getInstanceManager().registerInstance(lobby);

        // COMMAND
        CommandManager commandManager = MinecraftServer.getCommandManager();
        commandManager.setUnknownCommandCallback((sender, c) -> sender.sendMessage("Unknown command: " + c));
        commandManager.register(new StopCommand());
        commandManager.register(new LeaveCommand(this));
        commandManager.register(new CreateArenaCommand(this));
        commandManager.register(new JoinArenaCommand(this));
        commandManager.register(new ListPlayerCommand());

        // GLOBAL EVENT
        EventNode<Event> globalNode = MinecraftServer.getGlobalEventHandler();
        globalNode
                .addListener(PlayerLoginEvent.class, event -> {
                    var player = event.getPlayer();
                    for (var permission : mainConfig.getPlayerPermissions(player.getUsername())) {
                        player.addPermission(permission);
                    }
                });
        registerMOTDEvent(globalNode);

        // HOOK
        Board.hook(globalNode);
        LoginLogHook.hook(globalNode);
        UpdateViewHook.hook(globalNode);
        PerInstanceInstanceViewHook.hook(globalNode);
        PvpExtension.init();

        // Player
        MinecraftServer.getConnectionManager().setPlayerProvider(GamePlayer::new);

        // Console
        var consoleSender = commandManager.getConsoleSender();
        for (var permission : mainConfig.getConsolePermissions()) {
            consoleSender.addPermission(permission);
        }

        // Replacement
        ReplacementManager.addPlayerReplacement("player", Player::getName);
        ReplacementManager.addPlayerReplacement("ping", player -> Component.text(Integer.toString(player.getLatency())));
        ReplacementManager.addGlobalReplacement("online", () -> Component.text(Integer.toString(MinecraftServer.getConnectionManager().getOnlinePlayers().size())));
        mainConfig.getCustomPlaceholders().forEach((k, v) -> ReplacementManager.addGlobalReplacement(k, () -> v));
    }

    @ApiStatus.Internal
    void start() {
        ProxyType proxyType = mainConfig.getProxyType();
        proxyType.execute(this);

        MinecraftServer.setCompressionThreshold(mainConfig.getCompressionThreshold());
        MinecraftServer.setBrandName(mainConfig.getServerBrand());
        try {
            minecraftServer.start(mainConfig.getServerIp(), mainConfig.getServerPort());
        } catch (Exception e) {
            MinecraftServer.LOGGER.error("Failed to start server", e);
            System.exit(1);
            return;
        }
        if (mainConfig.isLANsupported()) {
            var config = new OpenToLANConfig().port(mainConfig.getServerPort());
            OpenToLAN.open(config);
        }

        lobby.init();

        arenaManager.init();
        arenaManager.postInit();

        Runtime.getRuntime().addShutdownHook(new Thread(this::stop));
    }

    @ApiStatus.Internal
    void stop() {
        lobby.clear();
        arenaManager.clear();
        MinecraftServer.stopCleanly();
    }

    private void registerMOTDEvent(EventNode<Event> node) {
        Component motd = getMotd();
        String favicon = getFavicon();
        node.addListener(ServerListPingEvent.class, event -> {
            ResponseData responseData = event.getResponseData();
            Collection<Player> players = MinecraftServer.getConnectionManager().getOnlinePlayers();
            if (Boolean.TRUE.equals(mainConfig.isShowPlayers())) {
                responseData.addEntries(players);
            }
            responseData.setOnline(players.size());
            responseData.setMaxPlayer(players.size() + 1);
            responseData.setDescription(motd);
            if (favicon != null) {
                responseData.setFavicon(favicon);
            }
        });
    }

    private Component getMotd() {
        List<Component> motd = mainConfig.getServerMOTD();
        if (motd.isEmpty()) {
            return Component.empty();
        }
        Component component = motd.get(0);
        for (int i = 1; i < motd.size(); i++) {
            component = component.append(Component.newline()).append(motd.get(i));
        }
        return component;
    }

    private String getFavicon() {
        String favicon = mainConfig.getServerFavicon();
        if (favicon.isBlank()) {
            return null;
        }
        if (favicon.startsWith("data:image/png;base64,")) {
            return favicon;
        }
        File file = AssetUtil.getAssetFile(favicon);
        if (!file.exists()) {
            return null;
        }
        try (FileInputStream inputStream = new FileInputStream(file)) {
            byte[] bytes = inputStream.readAllBytes();
            return "data:image/png;base64," + Base64.getEncoder().encodeToString(bytes);
        } catch (Exception e) {
            MinecraftServer.LOGGER.error("Failed to read favicon", e);
            return null;
        }
    }
}
