package me.hsgamer.epicmegagames;

import me.hsgamer.epicmegagames.command.StopCommand;
import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.config.MainConfig;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.CommandManager;
import net.minestom.server.event.player.PlayerLoginEvent;
import net.minestom.server.extras.bungee.BungeeCordProxy;

public class EpicMegaGames {
    private static final MainConfig mainConfig = new MainConfig();
    private static final LobbyConfig lobbyConfig = new LobbyConfig();

    static {
        mainConfig.setup();
        lobbyConfig.setup();
    }

    public static void main(String[] args) {
        MinecraftServer minecraftServer = MinecraftServer.init();

        registerCommands();

        MinecraftServer.getGlobalEventHandler().addListener(PlayerLoginEvent.class, event -> {
            event.setSpawningInstance(Lobby.INSTANCE);
            event.getPlayer().setRespawnPoint(Lobby.INSTANCE.getPosition());
        });

        if (MainConfig.BUNGEE.getValue()) {
            BungeeCordProxy.enable();
        }
        minecraftServer.start(MainConfig.SERVER_IP.getValue(), MainConfig.SERVER_PORT.getValue());
    }

    private static void registerCommands() {
        CommandManager manager = MinecraftServer.getCommandManager();
        manager.setUnknownCommandCallback((sender, c) -> sender.sendMessage("Unknown command: " + c));
        manager.register(new StopCommand());
    }
}
