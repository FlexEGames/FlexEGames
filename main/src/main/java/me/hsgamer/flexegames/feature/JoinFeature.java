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

public class JoinFeature extends ArenaFeature<JoinFeature.ArenaJoinFeature> {
    @Setter
    private BiFunction<Player, Arena, JoinResponse> joinResponseFunction = (p, a) -> new JoinResponse(false, Component.empty());
    @Setter
    private Function<Arena, Collection<Player>> playersFunction = a -> Collections.emptyList();
    @Setter
    private BiPredicate<Player, Arena> isJoinedPredicate = (p, a) -> playersFunction.apply(a).contains(p);
    @Setter
    private ToIntFunction<Arena> maxPlayersFunction = a -> 0;

    @Override
    protected ArenaJoinFeature createFeature(Arena arena) {
        return new ArenaJoinFeature(arena);
    }

    public class ArenaJoinFeature implements Feature {
        private final Arena arena;

        public ArenaJoinFeature(Arena arena) {
            this.arena = arena;
        }

        public Collection<Player> getPlayers() {
            return playersFunction.apply(arena);
        }

        public int getPlayerCount() {
            return getPlayers().size();
        }

        public int getMaxPlayers() {
            return maxPlayersFunction.applyAsInt(arena);
        }

        public JoinResponse join(Player player) {
            return joinResponseFunction.apply(player, arena);
        }

        public boolean isJoined(Player player) {
            return isJoinedPredicate.test(player, arena);
        }
    }
}
