package me.hsgamer.epicmegagames.feature;

import lombok.Getter;
import lombok.Setter;
import me.hsgamer.epicmegagames.api.ArenaGame;
import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;

import java.util.UUID;

public class GameFeature extends ArenaFeature<GameFeature.ArenaGameFeature> {

    @Override
    protected ArenaGameFeature createFeature(Arena arena) {
        return new ArenaGameFeature(arena);
    }

    public static class ArenaGameFeature implements Feature {
        private final Arena arena;
        @Getter
        private ArenaGame game;
        @Getter
        @Setter
        private UUID owner;

        public ArenaGameFeature(Arena arena) {
            this.arena = arena;
        }

        public void setGame(Template template) {
            this.game = template.createGame(arena);
        }
    }
}
