package me.hsgamer.epicmegagames;

import me.hsgamer.epicmegagames.command.StopCommand;
import me.hsgamer.epicmegagames.config.ChatConfig;
import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.config.MainConfig;
import me.hsgamer.epicmegagames.hook.ChatHook;
import me.hsgamer.epicmegagames.hook.ServerListHook;
import me.hsgamer.epicmegagames.lobby.Lobby;
import me.hsgamer.epicmegagames.manager.GameArenaManager;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.CommandManager;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerLoginEvent;
import net.minestom.server.extras.bungee.BungeeCordProxy;

public class GameServer {
    private final MainConfig mainConfig = new MainConfig();
    private final LobbyConfig lobbyConfig = new LobbyConfig();
    private final ChatConfig chatConfig = new ChatConfig();
    private final GameArenaManager gameArenaManager = new GameArenaManager();
    private final MinecraftServer minecraftServer = MinecraftServer.init();

    public GameServer() {
        // COMMAND
        CommandManager manager = MinecraftServer.getCommandManager();
        manager.setUnknownCommandCallback((sender, c) -> sender.sendMessage("Unknown command: " + c));
        manager.register(new StopCommand());

        // GLOBAL EVENT
        EventNode<Event> globalNode = MinecraftServer.getGlobalEventHandler();
        globalNode.addListener(PlayerLoginEvent.class, event -> {
            event.setSpawningInstance(Lobby.INSTANCE);
            event.getPlayer().setRespawnPoint(Lobby.INSTANCE.getPosition());
        });

        // HOOK
        ChatHook.hook(globalNode);
        ServerListHook.hook(globalNode);
    }

    public void load() {
        mainConfig.setup();
        lobbyConfig.setup();
        chatConfig.setup();
    }

    public void enable() {
        gameArenaManager.init();
        gameArenaManager.postInit();
    }

    public void disable() {
        gameArenaManager.clear();
    }

    public void start() {
        if (Boolean.TRUE.equals(MainConfig.BUNGEE.getValue())) {
            BungeeCordProxy.enable();
        }
        minecraftServer.start(MainConfig.SERVER_IP.getValue(), MainConfig.SERVER_PORT.getValue());
    }

    public MinecraftServer getMinecraftServer() {
        return minecraftServer;
    }

    public GameArenaManager getGameArenaManager() {
        return gameArenaManager;
    }
}
