package me.hsgamer.epicmegagames.feature;

import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.api.TemplateProvider;
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
        private Template template;

        public ArenaTemplateFeature(Arena arena) {
            this.arena = arena;
        }

        public Template getTemplate() {
            return template;
        }

        public void setTemplate(TemplateProvider provider) {
            this.template = provider.createTemplate(arena);
        }
    }
}
