package me.hsgamer.epicmegagames.builder;

import me.hsgamer.epicmegagames.api.TemplateProvider;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.config.Config;

public class TemplateProviderBuilder extends Builder<Config, TemplateProvider> {
    public static final TemplateProviderBuilder INSTANCE = new TemplateProviderBuilder();

    private TemplateProviderBuilder() {
    }
}
