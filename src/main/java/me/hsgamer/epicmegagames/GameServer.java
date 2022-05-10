package me.hsgamer.epicmegagames;

import lombok.Getter;
import me.hsgamer.epicmegagames.board.Board;
import me.hsgamer.epicmegagames.command.*;
import me.hsgamer.epicmegagames.config.ChatConfig;
import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.config.MainConfig;
import me.hsgamer.epicmegagames.config.MessageConfig;
import me.hsgamer.epicmegagames.hook.PerInstanceInstanceViewHook;
import me.hsgamer.epicmegagames.hook.PvpHook;
import me.hsgamer.epicmegagames.hook.ServerListHook;
import me.hsgamer.epicmegagames.hook.TickMonitorHook;
import me.hsgamer.epicmegagames.lobby.Lobby;
import me.hsgamer.epicmegagames.manager.GameArenaManager;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import me.hsgamer.epicmegagames.manager.TemplateManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.CommandManager;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerChatEvent;
import net.minestom.server.event.player.PlayerLoginEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.extras.MojangAuth;
import net.minestom.server.extras.PlacementRules;
import net.minestom.server.extras.bungee.BungeeCordProxy;
import net.minestom.server.extras.optifine.OptifineSupport;
import net.minestom.server.extras.velocity.VelocityProxy;
import org.jetbrains.annotations.ApiStatus;

import java.util.Map;
import java.util.Objects;

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
        // CONFIG
        mainConfig.setup();
        lobbyConfig.setup();
        chatConfig.setup();
        messageConfig.setup();

        // LOBBY
        lobby = new Lobby(this);
        MinecraftServer.getInstanceManager().registerInstance(lobby);

        // COMMAND
        CommandManager manager = MinecraftServer.getCommandManager();
        manager.setUnknownCommandCallback((sender, c) -> sender.sendMessage("Unknown command: " + c));
        manager.register(new StopCommand(this));
        manager.register(new LeaveCommand(this));
        manager.register(new CreateArenaCommand(this));
        manager.register(new JoinArenaCommand(this));
        manager.register(new ListPlayerCommand());
        manager.register(new TickMonitorCommand());

        // GLOBAL EVENT
        EventNode<Event> globalNode = MinecraftServer.getGlobalEventHandler();
        globalNode
                .addListener(PlayerLoginEvent.class, event -> {
                    event.setSpawningInstance(lobby);
                    event.getPlayer().setRespawnPoint(lobby.getPosition());
                })
                .addListener(PlayerChatEvent.class, event -> {
                    event.setChatFormat(e -> ReplacementManager.builder()
                            .replaceGlobal()
                            .replacePlayer(event.getPlayer())
                            .replace(Map.of("message", () -> Component.text(e.getMessage())))
                            .build(ChatConfig.CHAT_FORMAT.getValue())
                    );
                    event.getRecipients().removeIf(p -> !Objects.equals(p.getInstance(), event.getPlayer().getInstance()));
                })
                .addListener(PlayerSpawnEvent.class, event -> event.getPlayer().refreshCommands());

        // HOOK
        ServerListHook.hook(globalNode);
        PerInstanceInstanceViewHook.hook(globalNode);
        Board.hook(globalNode);
        PvpHook.hook(globalNode);
        TickMonitorHook.hook();
        PlacementRules.init();
        OptifineSupport.enable();

        // Replacement
        ReplacementManager.addPlayerReplacement("player", Player::getName);
        ReplacementManager.addGlobalReplacement("online", () -> Component.text(Integer.toString(MinecraftServer.getConnectionManager().getOnlinePlayers().size())));
    }

    public void enable() {
        templateManager.init();
        gameArenaManager.init();
        gameArenaManager.postInit();
    }

    public void disable() {
        gameArenaManager.clear();
        lobby.clear();
    }

    @ApiStatus.Internal
    public void start() {
        enable();
        String secret = MainConfig.VELOCITY_SECRET.getValue();
        if (!secret.isBlank()) {
            VelocityProxy.enable(secret);
        } else if (Boolean.TRUE.equals(MainConfig.BUNGEE.getValue())) {
            BungeeCordProxy.enable();
        }
        if (Boolean.TRUE.equals(MainConfig.SERVER_ONLINE_MODE.getValue())) {
            MojangAuth.init();
        }
        MinecraftServer.setCompressionThreshold(MainConfig.COMPRESSION_THRESHOLD.getValue());
        MinecraftServer.setBrandName(MainConfig.BRAND_NAME.getValue());
        try {
            minecraftServer.start(MainConfig.SERVER_IP.getValue(), MainConfig.SERVER_PORT.getValue());
        } catch (Exception e) {
            MinecraftServer.LOGGER.error("Failed to start server", e);
            System.exit(1);
        }
    }

    @ApiStatus.Internal
    public void stop() {
        MinecraftServer.stopCleanly();
        disable();
    }
}
