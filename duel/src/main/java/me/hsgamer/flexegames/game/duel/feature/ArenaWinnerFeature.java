package me.hsgamer.flexegames.game.duel.feature;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

public class ArenaWinnerFeature implements Feature {
    private final Arena arena;
    private final AtomicReference<Player> winner = new AtomicReference<>();

    public ArenaWinnerFeature(Arena arena) {
        this.arena = arena;
    }

    public boolean checkWinner() {
        var feature = arena.getArenaFeature(InstanceFeature.class);
        List<Player> alivePlayers = feature.getAlivePlayers();
        if (alivePlayers.size() <= 1) {
            if (alivePlayers.size() == 1) {
                winner.set(alivePlayers.get(0));
            }
            return true;
        }
        return false;
    }

    public Player getWinner() {
        return winner.get();
    }
}
