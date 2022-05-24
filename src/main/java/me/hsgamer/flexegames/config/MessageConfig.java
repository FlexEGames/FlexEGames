package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.path.ComponentPath;
import me.hsgamer.flexegames.config.path.StringComponentListPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;

import java.io.File;
import java.util.List;

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

    public static final ConfigPath<Component> LOBBY_HIDE_PLAYERS = new ComponentPath("lobby.hide-players", "&aYou now &ehide &aother players");
    public static final ConfigPath<Component> LOBBY_SHOW_PLAYERS = new ComponentPath("lobby.show-players", "&aYou now &eshow &aother players");

    public static final ConfigPath<Component> GAME_DUEL_BOARD_TITLE = new ComponentPath("game.duel.board.title", "&e&lDuel");
    public static final ConfigPath<List<Component>> GAME_DUEL_BOARD_LINES_WAITING = new StringComponentListPath("game.duel.board.lines.waiting", List.of(
            "&eWaiting for players",
            "&ePlayers: &a%players%",
            "&eTime Left: &a%time%"
    ));
    public static final ConfigPath<List<Component>> GAME_DUEL_BOARD_LINES_INGAME = new StringComponentListPath("game.duel.board.lines.ingame", List.of(
            "&eAlive: &a%alive%"
    ));
    public static final ConfigPath<List<Component>> GAME_DUEL_BOARD_LINES_ENDING = new StringComponentListPath("game.duel.board.lines.ending", List.of(
            "&eEnding in: &a%time%",
            "&eWinner: &a%winner%"
    ));

    public MessageConfig() {
        super(new YamlProvider().loadConfiguration(new File("messages.yml")));
    }
}
