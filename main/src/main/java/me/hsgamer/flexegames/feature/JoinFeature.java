package me.hsgamer.flexegames.feature;

import lombok.Setter;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

import java.util.Collection;
import java.util.Collections;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.ToIntFunction;

/**
 * The feature to join an {@link Arena}
 */
public class JoinFeature extends ArenaFeature<JoinFeature.ArenaJoinFeature> {
    /**
     * The function to handle the join request of a {@link Player} to an {@link Arena} and return the {@link JoinResponse}
     */
    @Setter
    private BiFunction<Player, Arena, JoinResponse> joinResponseFunction = (p, a) -> new JoinResponse(false, Component.empty());
    /**
     * The function to provide the players in an {@link Arena}
     */
    @Setter
    private Function<Arena, Collection<Player>> playersFunction = a -> Collections.emptyList();
    /**
     * The predicate to check if a {@link Player} already joins an {@link Arena}
     */
    @Setter
    private BiPredicate<Player, Arena> isJoinedPredicate = (p, a) -> playersFunction.apply(a).contains(p);
    /**
     * The function to provide the maximum players of an {@link Arena}
     */
    @Setter
    private ToIntFunction<Arena> maxPlayersFunction = a -> 0;

    @Override
    protected ArenaJoinFeature createFeature(Arena arena) {
        return new ArenaJoinFeature(arena);
    }

    /**
     * The feature to join the specific {@link Arena}
     */
    public class ArenaJoinFeature implements Feature {
        private final Arena arena;

        public ArenaJoinFeature(Arena arena) {
            this.arena = arena;
        }

        /**
         * Get the collection of {@link Player} in the {@link Arena}
         *
         * @return the collection of {@link Player}
         */
        public Collection<Player> getPlayers() {
            return playersFunction.apply(arena);
        }

        /**
         * Get the player count in the {@link Arena}
         *
         * @return the player count
         */
        public int getPlayerCount() {
            return getPlayers().size();
        }

        /**
         * Get the maximum players in the {@link Arena}
         *
         * @return the maximum players
         */
        public int getMaxPlayers() {
            return maxPlayersFunction.applyAsInt(arena);
        }

        /**
         * Handle the join request of a {@link Player} to the {@link Arena}
         *
         * @param player the {@link Player}
         * @return the {@link JoinResponse}
         */
        public JoinResponse join(Player player) {
            return joinResponseFunction.apply(player, arena);
        }

        /**
         * Check if a {@link Player} already joins the {@link Arena}
         *
         * @param player the {@link Player}
         * @return true if the {@link Player} already joins the {@link Arena}
         */
        public boolean isJoined(Player player) {
            return isJoinedPredicate.test(player, arena);
        }
    }
}
