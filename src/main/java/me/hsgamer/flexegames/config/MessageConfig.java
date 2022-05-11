package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.path.ComponentListPath;
import me.hsgamer.flexegames.config.path.ComponentPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class MessageConfig extends PathableConfig {
    public static final ConfigPath<List<Component>> TICK_MONITOR = new ComponentListPath("tick-monitor", Arrays.asList(
            Component.text("&7Tick: &c%tick_time%"),
            Component.text("&7RAM: &c%ram_usage%"),
            Component.text("&7Online: &c%online%")
    ));
    public static final ConfigPath<Component> ERROR_TEMPLATE_NOT_FOUND = new ComponentPath("error.template-not-found", Component.text("&cTemplate not found"));
    public static final ConfigPath<Component> ERROR_ARENA_NOT_FOUND = new ComponentPath("error.arena-not-found", Component.text("&cArena not found"));
    public static final ConfigPath<Component> ERROR_ARENA_NOT_SETUP = new ComponentPath("error.arena-not-setup", Component.text("&cArena not setup"));
    public static final ConfigPath<Component> RESPONSE_MAX_PLAYERS_REACH = new ComponentPath("response.max-players-reach", Component.text("&cMax players reached"));
    public static final ConfigPath<Component> RESPONSE_INCOMPLETE_SETUP = new ComponentPath("response.incomplete-setup", Component.text("&cIncomplete setup"));
    public static final ConfigPath<Component> RESPONSE_NOT_WAITING = new ComponentPath("response.not-waiting", Component.text("&cArena not waiting"));

    public static final ConfigPath<String> STATE_CHOOSING = Paths.stringPath("state.choosing", "Choosing");
    public static final ConfigPath<String> STATE_WAITING = Paths.stringPath("state.waiting", "Waiting");
    public static final ConfigPath<String> STATE_IN_GAME = Paths.stringPath("state.in-game", "In game");
    public static final ConfigPath<String> STATE_ENDING = Paths.stringPath("state.ending", "Ending");
    public static final ConfigPath<String> STATE_KILLING = Paths.stringPath("state.killing", "Killing");

    public MessageConfig() {
        super(new YamlProvider().loadConfiguration(new File("messages.yml")));
    }
}
