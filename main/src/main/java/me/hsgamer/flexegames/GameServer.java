package me.hsgamer.flexegames;

import io.github.bloepiloepi.pvp.PvpExtension;
import lombok.Getter;
import me.hsgamer.flexegames.command.*;
import me.hsgamer.flexegames.config.ChatConfig;
import me.hsgamer.flexegames.config.LobbyConfig;
import me.hsgamer.flexegames.config.MainConfig;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.hook.ChatHook;
import me.hsgamer.flexegames.hook.LoginLogHook;
import me.hsgamer.flexegames.hook.ServerListHook;
import me.hsgamer.flexegames.hook.UpdateViewHook;
import me.hsgamer.flexegames.lobby.Lobby;
import me.hsgamer.flexegames.manager.GameArenaManager;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.manager.TemplateManager;
import me.hsgamer.flexegames.player.GamePlayer;
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
import org.jetbrains.annotations.ApiStatus;

@Getter
public class GameServer {
    private final MinecraftServer minecraftServer = MinecraftServer.init();
    private final MainConfig mainConfig = new MainConfig();
    private final LobbyConfig lobbyConfig = new LobbyConfig();
    private final ChatConfig chatConfig = new ChatConfig();
    private final MessageConfig messageConfig = new MessageConfig();
    private final GameArenaManager gameArenaManager = new GameArenaManager(this);
    private final TemplateManager templateManager = new TemplateManager();
    private final Lobby lobby;

    public GameServer() {
        // Redirect System.out and System.err to the logger
        SysOutErrRedirect.init();

        // Redirect Throwable handler
        MinecraftServer.getExceptionManager().setExceptionHandler(throwable ->
                MinecraftServer.LOGGER.error("Throwable: " + throwable.getMessage(), throwable)
        );

        // CONFIG
        mainConfig.setup();
        lobbyConfig.setup();
        chatConfig.setup();
        messageConfig.setup();

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
                    for (var permission : MainConfig.getPlayerPermissions(player.getUsername())) {
                        player.addPermission(permission);
                    }
                });

        // HOOK
        ServerListHook.hook(globalNode);
        Board.hook(globalNode);
        ChatHook.hook(globalNode);
        LoginLogHook.hook(globalNode);
        UpdateViewHook.hook(globalNode);
        PvpExtension.init();

        // Player
        MinecraftServer.getConnectionManager().setPlayerProvider(GamePlayer::new);

        // Console
        var consoleSender = commandManager.getConsoleSender();
        for (var permission : MainConfig.CONSOLE_PERMISSIONS.getValue()) {
            consoleSender.addPermission(permission);
        }

        // Replacement
        ReplacementManager.addPlayerReplacement("player", Player::getName);
        ReplacementManager.addPlayerReplacement("ping", player -> Component.text(Integer.toString(player.getLatency())));
        ReplacementManager.addGlobalReplacement("online", () -> Component.text(Integer.toString(MinecraftServer.getConnectionManager().getOnlinePlayers().size())));
        MainConfig.CUSTOM_PLACEHOLDERS.getValue().forEach((k, v) -> ReplacementManager.addGlobalReplacement(k, () -> v));
    }

    @ApiStatus.Internal
    public void start() {
        templateManager.prepare();

        ProxyType proxyType = MainConfig.SERVER_TYPE.getValue();
        proxyType.execute();

        MinecraftServer.setCompressionThreshold(MainConfig.SERVER_COMPRESSION_THRESHOLD.getValue());
        MinecraftServer.setBrandName(MainConfig.SERVER_BRAND_NAME.getValue());
        try {
            minecraftServer.start(MainConfig.SERVER_IP.getValue(), MainConfig.SERVER_PORT.getValue());
        } catch (Exception e) {
            MinecraftServer.LOGGER.error("Failed to start server", e);
            System.exit(1);
        }

        templateManager.init();
        gameArenaManager.init();
        gameArenaManager.postInit();
    }

    @ApiStatus.Internal
    public void stop() {
        gameArenaManager.clear();
        lobby.clear();
        MinecraftServer.stopCleanly();
    }
}
