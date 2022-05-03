package me.hsgamer.epicmegagames.feature;

import me.hsgamer.epicmegagames.api.GameTemplate;
import me.hsgamer.epicmegagames.api.GameTemplateProvider;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;

public class TemplateFeature extends ArenaFeature<TemplateFeature.ArenaTemplateFeature> {


    @Override
    protected ArenaTemplateFeature createFeature(Arena arena) {
        return new ArenaTemplateFeature(arena);
    }

    public static class ArenaTemplateFeature implements Feature {
        private final Arena arena;
        private GameTemplate template;

        public ArenaTemplateFeature(Arena arena) {
            this.arena = arena;
        }

        public GameTemplate getTemplate() {
            return template;
        }

        public void setTemplate(GameTemplateProvider provider) {
            this.template = provider.createTemplate(arena);
        }
    }
}
