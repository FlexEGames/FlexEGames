package me.hsgamer.flexgames.feature;

import me.hsgamer.flexgames.GameServer;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

public record LobbyFeature(GameServer gameServer) implements Feature {
    public void backToLobby(Player player) {
        player.setInstance(gameServer.getLobby());
    }
}
