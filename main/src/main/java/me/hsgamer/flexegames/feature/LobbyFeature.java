package me.hsgamer.flexegames.feature;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

public final class LobbyFeature implements Feature {
    private final GameServer gameServer;

    public LobbyFeature(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    public void backToLobby(Player player) {
        player.setInstance(gameServer.getLobby(), gameServer.getLobby().getPosition());
    }
}
