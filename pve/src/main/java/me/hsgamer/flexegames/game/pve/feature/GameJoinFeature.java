package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.state.WaitingState;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.entity.Player;

import java.util.Collection;

public class GameJoinFeature implements JoinFeature {
    private final Arena arena;
    private final PveExtension pveExtension;

    public GameJoinFeature(Arena arena, PveExtension pveExtension) {
        this.arena = arena;
        this.pveExtension = pveExtension;
    }

    @Override
    public Collection<Player> getPlayers() {
        return arena.getFeature(InstanceFeature.class).getInstance().getPlayers();
    }

    @Override
    public int getMaxPlayers() {
        return pveExtension.getMainConfig().getMaxPlayers();
    }

    @Override
    public JoinResponse join(Player player) {
        if (arena.getCurrentState() != WaitingState.class) {
            return JoinResponse.fail(pveExtension.getMessageConfig().getNotWaiting());
        }
        var instanceFeature = arena.getFeature(InstanceFeature.class);
        var instance = instanceFeature.getInstance();
        if (instance.getPlayers().size() >= getMaxPlayers()) {
            return JoinResponse.fail(pveExtension.getMessageConfig().getMaxPlayersReached());
        }
        player.setInstance(instance);
        return JoinResponse.successful();
    }

    @Override
    public boolean isJoined(Player player) {
        return arena.getFeature(InstanceFeature.class).getInstance() == player.getInstance();
    }
}
