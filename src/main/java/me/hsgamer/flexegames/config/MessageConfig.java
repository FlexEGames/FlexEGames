package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.path.ComponentPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;

import java.io.File;

public class MessageConfig extends PathableConfig {
    public static final ConfigPath<Component> ERROR_TEMPLATE_NOT_FOUND = new ComponentPath("error.template-not-found", "&cTemplate not found");
    public static final ConfigPath<Component> ERROR_ARENA_NOT_FOUND = new ComponentPath("error.arena-not-found", "&cArena not found");
    public static final ConfigPath<Component> ERROR_ARENA_NOT_SETUP = new ComponentPath("error.arena-not-setup", "&cArena not setup");
    public static final ConfigPath<Component> RESPONSE_MAX_PLAYERS_REACH = new ComponentPath("response.max-players-reach", "&cMax players reached");
    public static final ConfigPath<Component> RESPONSE_INCOMPLETE_SETUP = new ComponentPath("response.incomplete-setup", "&cIncomplete setup");
    public static final ConfigPath<Component> RESPONSE_NOT_WAITING = new ComponentPath("response.not-waiting", "&cArena not waiting");
    public static final ConfigPath<Component> RESPONSE_CREATE_ARENA_SUCCESSFUL = new ComponentPath("response.create-arena-successful", "&aArena created successfully");
    public static final ConfigPath<Component> RESPONSE_CANNOT_CREATE_ARENA = new ComponentPath("response.cannot-create-arena", "&cYou cannot create arena");

    public static final ConfigPath<String> STATE_CHOOSING = Paths.stringPath("state.choosing", "Choosing");
    public static final ConfigPath<String> STATE_WAITING = Paths.stringPath("state.waiting", "Waiting");
    public static final ConfigPath<String> STATE_IN_GAME = Paths.stringPath("state.in-game", "In game");
    public static final ConfigPath<String> STATE_ENDING = Paths.stringPath("state.ending", "Ending");
    public static final ConfigPath<String> STATE_KILLING = Paths.stringPath("state.killing", "Killing");

    public static final ConfigPath<Component> LOBBY_HIDE_PLAYERS = new ComponentPath("lobby.hide-players", "&aYou now &ehide &a other players");
    public static final ConfigPath<Component> LOBBY_SHOW_PLAYERS = new ComponentPath("lobby.show-players", "&aYou now &eshow &a other players");

    public MessageConfig() {
        super(new YamlProvider().loadConfiguration(new File("messages.yml")));
    }
}
