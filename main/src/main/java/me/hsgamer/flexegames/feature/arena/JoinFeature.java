package me.hsgamer.flexegames.feature.arena;

import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

import java.util.Collection;

/**
 * The feature to join the specific {@link Arena}
 */
public interface JoinFeature extends Feature {
    /**
     * Get the collection of {@link Player} in the {@link Arena}
     *
     * @return the collection of {@link Player}
     */
    Collection<Player> getPlayers();

    /**
     * Get the player count in the {@link Arena}
     *
     * @return the player count
     */
    default int getPlayerCount() {
        return getPlayers().size();
    }

    /**
     * Get the maximum players in the {@link Arena}
     *
     * @return the maximum players
     */
    int getMaxPlayers();

    /**
     * Handle the join request of a {@link Player} to the {@link Arena}
     *
     * @param player the {@link Player}
     * @return the {@link JoinResponse}
     */
    JoinResponse join(Player player);

    /**
     * Check if a {@link Player} already joins the {@link Arena}
     *
     * @param player the {@link Player}
     * @return true if the {@link Player} already joins the {@link Arena}
     */
    boolean isJoined(Player player);
}
