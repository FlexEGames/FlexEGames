package me.hsgamer.epicmegagames.feature;

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
        private ArenaGame arenaGame;
        private UUID owner;

        public ArenaGameFeature(Arena arena) {
            this.arena = arena;
        }

        public ArenaGame getGame() {
            return arenaGame;
        }

        public void setGame(Template template) {
            this.arenaGame = template.createGame(arena);
        }

        public UUID getOwner() {
            return owner;
        }

        public void setOwner(UUID owner) {
            this.owner = owner;
        }
    }
}
