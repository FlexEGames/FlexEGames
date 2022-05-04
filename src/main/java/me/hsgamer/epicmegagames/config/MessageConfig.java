package me.hsgamer.epicmegagames.config;

import me.hsgamer.epicmegagames.config.path.ComponentPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;

import java.io.File;

public class MessageConfig extends PathableConfig {
    public static final ConfigPath<Component> ERROR_TEMPLATE_NOT_FOUND = new ComponentPath("error.template-not-found", Component.text("&cTemplate not found"));
    public static final ConfigPath<Component> RESPONSE_MAX_PLAYERS_REACH = new ComponentPath("response.max-players-reach", Component.text("&cMax players reached"));
    public static final ConfigPath<Component> RESPONSE_INCOMPLETE_SETUP = new ComponentPath("response.incomplete-setup", Component.text("&cIncomplete setup"));

    public MessageConfig() {
        super(new YamlProvider().loadConfiguration(new File("messages.yml")));
    }
}
