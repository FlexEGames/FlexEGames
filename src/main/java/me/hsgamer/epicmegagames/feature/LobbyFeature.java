package me.hsgamer.epicmegagames.feature;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

public record LobbyFeature(GameServer gameServer) implements Feature {
    public void backToLobby(Player player) {
        player.setInstance(gameServer.getLobby());
    }
}
