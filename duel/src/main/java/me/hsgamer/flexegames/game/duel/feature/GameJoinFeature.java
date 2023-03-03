package me.hsgamer.flexegames.game.duel.feature;

import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.state.WaitingState;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.entity.Player;

import java.util.Collection;

public class GameJoinFeature implements JoinFeature {
    private final Arena arena;
    private final DuelExtension duelExtension;

    public GameJoinFeature(Arena arena, DuelExtension duelExtension) {
        this.arena = arena;
        this.duelExtension = duelExtension;
    }

    @Override
    public Collection<Player> getPlayers() {
        return arena.getFeature(InstanceFeature.class).getInstance().getPlayers();
    }

    @Override
    public int getMaxPlayers() {
        return arena.getFeature(InstanceFeature.class).getDuelWorld().getPos().size();
    }

    @Override
    public JoinResponse join(Player player) {
        if (arena.getCurrentState() != WaitingState.class) {
            return JoinResponse.fail(duelExtension.getMessageConfig().getNotWaiting());
        }
        var instanceFeature = arena.getFeature(InstanceFeature.class);
        var instance = instanceFeature.getInstance();
        if (instance.getPlayers().size() >= getMaxPlayers()) {
            return JoinResponse.fail(duelExtension.getMessageConfig().getMaxPlayersReached());
        }
        player.setInstance(instance);
        return JoinResponse.successful();
    }

    @Override
    public boolean isJoined(Player player) {
        return arena.getFeature(InstanceFeature.class).getInstance() == player.getInstance();
    }
}
