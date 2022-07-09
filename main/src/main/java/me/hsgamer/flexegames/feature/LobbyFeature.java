package me.hsgamer.flexegames.feature;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

public record LobbyFeature(GameServer gameServer) implements Feature {
    public void backToLobby(Player player) {
        player.setInstance(gameServer.getLobby(), gameServer.getLobby().getPosition());
    }
}
