package me.hsgamer.flexegames.feature;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

import java.util.Collection;
import java.util.List;

public final class LobbyFeature implements Feature {
    private final GameServer gameServer;

    public LobbyFeature(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    public void send(Collection<Player> players) {
        for (Player player : players) {
            player.setInstance(gameServer.getLobby(), gameServer.getLobby().getPosition());
        }
    }

    public void send(Player... players) {
        send(List.of(players));
    }
}
