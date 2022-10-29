package me.hsgamer.flexegames.feature;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

import java.util.Collection;
import java.util.List;

/**
 * The feature to teleport players to the {@link me.hsgamer.flexegames.lobby.Lobby}
 */
public final class LobbyFeature implements Feature {
    private final GameServer gameServer;

    public LobbyFeature(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    /**
     * Teleport players to the {@link me.hsgamer.flexegames.lobby.Lobby}
     *
     * @param players the players
     */
    public void send(Collection<Player> players) {
        for (Player player : players) {
            player.setInstance(gameServer.getLobby(), gameServer.getLobby().getPosition());
        }
    }

    /**
     * Teleport players to the {@link me.hsgamer.flexegames.lobby.Lobby}
     *
     * @param players the players
     */
    public void send(Player... players) {
        send(List.of(players));
    }
}
