package me.hsgamer.flexegames.builder;

import me.hsgamer.flexegames.api.Template;
import me.hsgamer.flexegames.template.duel.DuelTemplate;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.config.Config;

import java.util.Optional;

public class TemplateBuilder extends Builder<Config, Template> {
    public static final TemplateBuilder INSTANCE = new TemplateBuilder();

    private TemplateBuilder() {
        register(DuelTemplate::new, "duel");
    }

    public static Optional<Template> buildTemplate(Config config) {
        return Optional.ofNullable(config.get("type"))
                .map(String::valueOf)
                .flatMap(type -> TemplateBuilder.INSTANCE.build(type, config));
    }
}
