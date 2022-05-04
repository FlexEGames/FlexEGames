package me.hsgamer.epicmegagames.config;

import me.hsgamer.epicmegagames.config.path.LegacyComponentPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;

import java.io.File;

public class MessageConfig extends PathableConfig {
    public static final ConfigPath<Component> ERROR_TEMPLATE_NOT_FOUND = new LegacyComponentPath("error.template-not-found", Component.text("&cTemplate not found"));

    public MessageConfig() {
        super(new YamlProvider().loadConfiguration(new File("messages.yml")));
    }
}
