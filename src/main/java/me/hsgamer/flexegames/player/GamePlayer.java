package me.hsgamer.flexegames.player;

import io.github.bloepiloepi.pvp.entities.CustomPlayer;
import net.minestom.server.network.player.PlayerConnection;
import org.jetbrains.annotations.NotNull;

import java.util.UUID;

public class GamePlayer extends CustomPlayer {
    public GamePlayer(@NotNull UUID uuid, @NotNull String username, @NotNull PlayerConnection playerConnection) {
        super(uuid, username, playerConnection);
    }
}
