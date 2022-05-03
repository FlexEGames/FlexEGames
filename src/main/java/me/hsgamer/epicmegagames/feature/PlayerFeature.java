package me.hsgamer.epicmegagames.feature;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;

public class PlayerFeature extends ArenaFeature<PlayerFeature.ArenaPlayerFeature> {
    @Override
    protected ArenaPlayerFeature createFeature(Arena arena) {
        return new ArenaPlayerFeature(arena);
    }

    public static final class ArenaPlayerFeature implements Feature {
        private final Arena arena;

        public ArenaPlayerFeature(Arena arena) {
            this.arena = arena;
        }
    }
}
