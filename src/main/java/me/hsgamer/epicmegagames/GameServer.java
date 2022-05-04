package me.hsgamer.epicmegagames;

import me.hsgamer.epicmegagames.command.CreateArenaCommand;
import me.hsgamer.epicmegagames.command.LeaveCommand;
import me.hsgamer.epicmegagames.command.StopCommand;
import me.hsgamer.epicmegagames.config.ChatConfig;
import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.config.MainConfig;
import me.hsgamer.epicmegagames.hook.ServerListHook;
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
import net.minestom.server.extras.MojangAuth;
import net.minestom.server.extras.bungee.BungeeCordProxy;

import java.util.Map;
import java.util.Objects;

public class GameServer {
    private final MinecraftServer minecraftServer = MinecraftServer.init();
    private final MainConfig mainConfig = new MainConfig();
    private final LobbyConfig lobbyConfig = new LobbyConfig();
    private final ChatConfig chatConfig = new ChatConfig();
    private final GameArenaManager gameArenaManager = new GameArenaManager(this);
    private final TemplateManager templateManager = new TemplateManager();
    private final Lobby lobby;

    public GameServer() {
        // CONFIG
        mainConfig.setup();
        lobbyConfig.setup();
        chatConfig.setup();

        // LOBBY
        lobby = new Lobby();
        MinecraftServer.getInstanceManager().registerInstance(lobby);

        // COMMAND
        CommandManager manager = MinecraftServer.getCommandManager();
        manager.setUnknownCommandCallback((sender, c) -> sender.sendMessage("Unknown command: " + c));
        manager.register(new StopCommand());
        manager.register(new LeaveCommand(this));
        manager.register(new CreateArenaCommand(this));

        // GLOBAL EVENT
        EventNode<Event> globalNode = MinecraftServer.getGlobalEventHandler();
        globalNode.addListener(PlayerLoginEvent.class, event -> event.setSpawningInstance(lobby));
        globalNode.addListener(PlayerChatEvent.class, event -> {
            event.setChatFormat(e -> ReplacementManager.builder()
                    .replaceGlobal()
                    .replacePlayer(event.getPlayer())
                    .replace(Map.of("message", Component.text(e.getMessage())))
                    .build(ChatConfig.CHAT_FORMAT.getValue())
            );
            event.getRecipients().removeIf(p -> !Objects.equals(p.getInstance(), event.getPlayer().getInstance()));
        });

        // HOOK
        ServerListHook.hook(globalNode);

        // Replacement
        ReplacementManager.addPlayerReplacement("player", Player::getName);
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

    public void start() {
        if (Boolean.TRUE.equals(MainConfig.BUNGEE.getValue())) {
            BungeeCordProxy.enable();
        }
        if (Boolean.TRUE.equals(MainConfig.SERVER_ONLINE_MODE.getValue())) {
            MojangAuth.init();
        }
        minecraftServer.start(MainConfig.SERVER_IP.getValue(), MainConfig.SERVER_PORT.getValue());
    }

    public MinecraftServer getMinecraftServer() {
        return minecraftServer;
    }

    public Lobby getLobby() {
        return lobby;
    }

    public GameArenaManager getGameArenaManager() {
        return gameArenaManager;
    }

    public TemplateManager getTemplateManager() {
        return templateManager;
    }

    public MainConfig getMainConfig() {
        return mainConfig;
    }

    public ChatConfig getChatConfig() {
        return chatConfig;
    }

    public LobbyConfig getLobbyConfig() {
        return lobbyConfig;
    }
}
