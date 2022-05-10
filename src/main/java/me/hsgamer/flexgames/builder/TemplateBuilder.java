package me.hsgamer.flexgames.builder;

import me.hsgamer.flexgames.api.Template;
import me.hsgamer.flexgames.template.duel.DuelTemplate;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.config.Config;

public class TemplateBuilder extends Builder<Config, Template> {
    public static final TemplateBuilder INSTANCE = new TemplateBuilder();

    private TemplateBuilder() {
        register(DuelTemplate::new, "duel");
    }
}
