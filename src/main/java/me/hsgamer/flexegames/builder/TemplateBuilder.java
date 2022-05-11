package me.hsgamer.flexegames.builder;

import me.hsgamer.flexegames.api.Template;
import me.hsgamer.flexegames.template.duel.DuelTemplate;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.config.Config;

public class TemplateBuilder extends Builder<Config, Template> {
    public static final TemplateBuilder INSTANCE = new TemplateBuilder();

    private TemplateBuilder() {
        register(DuelTemplate::new, "duel");
    }
}
