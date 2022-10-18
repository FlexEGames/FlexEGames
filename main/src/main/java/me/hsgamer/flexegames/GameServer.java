package me.hsgamer.flexegames;

import io.github.bloepiloepi.pvp.PvpExtension;
import lombok.Getter;
import me.hsgamer.flexegames.command.*;
import me.hsgamer.flexegames.config.LobbyConfig;
import me.hsgamer.flexegames.config.MainConfig;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.hook.LoginLogHook;
import me.hsgamer.flexegames.hook.PerInstanceTabListHook;
import me.hsgamer.flexegames.hook.UpdateViewHook;
import me.hsgamer.flexegames.lobby.Lobby;
import me.hsgamer.flexegames.manager.GameManager;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.player.GamePlayer;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.flexegames.util.ProxyType;
import me.hsgamer.flexegames.util.SysOutErrRedirect;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.minestom.board.Board;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.CommandManager;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerLoginEvent;
import net.minestom.server.event.server.ServerListPingEvent;
import net.minestom.server.ping.ResponseData;
import org.jetbrains.annotations.ApiStatus;

import java.io.File;
import java.io.FileInputStream;
import java.util.Base64;
import java.util.Collection;
import java.util.List;

@Getter
public class GameServer {
    private final MinecraftServer minecraftServer = MinecraftServer.init();
    private final MainConfig mainConfig = YamlConfigGenerator.generate(MainConfig.class, new File("config.yml"), true, true);
    private final LobbyConfig lobbyConfig = YamlConfigGenerator.generate(LobbyConfig.class, new File("lobby.yml"), true, true);
    private final MessageConfig messageConfig = YamlConfigGenerator.generate(MessageConfig.class, new File("messages.yml"), true, true);
    private final GameManager gameManager = new GameManager(this);
    private final Lobby lobby;

    public GameServer() {
        // Redirect System.out and System.err to the logger
        SysOutErrRedirect.init();

        // Redirect Throwable handler
        MinecraftServer.getExceptionManager().setExceptionHandler(throwable ->
                MinecraftServer.LOGGER.error("Throwable: " + throwable.getMessage(), throwable)
        );

        // LOBBY
        lobby = new Lobby(this);
        MinecraftServer.getInstanceManager().registerInstance(lobby);
        lobby.init();

        // COMMAND
        CommandManager commandManager = MinecraftServer.getCommandManager();
        commandManager.setUnknownCommandCallback((sender, c) -> sender.sendMessage("Unknown command: " + c));
        commandManager.register(new StopCommand(this));
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
        PerInstanceTabListHook.hook(globalNode);
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
    public void start() {
        gameManager.prepare();

        ProxyType proxyType = mainConfig.getProxyType();
        proxyType.execute(this);

        MinecraftServer.setCompressionThreshold(mainConfig.getCompressionThreshold());
        MinecraftServer.setBrandName(mainConfig.getServerBrand());
        try {
            minecraftServer.start(mainConfig.getServerIp(), mainConfig.getServerPort());
        } catch (Exception e) {
            MinecraftServer.LOGGER.error("Failed to start server", e);
            System.exit(1);
        }

        gameManager.init();
    }

    @ApiStatus.Internal
    public void stop() {
        lobby.clear();
        gameManager.clear();
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
