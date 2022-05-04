package me.hsgamer.epicmegagames.builder;

import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.template.duel.DuelTemplate;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.config.Config;

public class TemplateBuilder extends Builder<Config, Template> {
    public static final TemplateBuilder INSTANCE = new TemplateBuilder();

    private TemplateBuilder() {
        register(DuelTemplate::new, "duel");
    }
}
