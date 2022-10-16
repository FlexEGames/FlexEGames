package me.hsgamer.flexegames.feature;

import lombok.Setter;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

import java.util.function.BiFunction;

public class JoinFeature extends ArenaFeature<JoinFeature.ArenaJoinFeature> {
    @Override
    protected ArenaJoinFeature createFeature(Arena arena) {
        return new ArenaJoinFeature(arena);
    }

    public static class ArenaJoinFeature implements Feature {
        private final Arena arena;
        @Setter
        private BiFunction<Player, Arena, JoinResponse> joinResponseFunction = (p, a) -> new JoinResponse(false, Component.empty());

        public ArenaJoinFeature(Arena arena) {
            this.arena = arena;
        }

        public JoinResponse join(Player player) {
            return joinResponseFunction.apply(player, arena);
        }
    }
}
