package me.hsgamer.epicmegagames.config;

import me.hsgamer.epicmegagames.config.path.ComponentListPath;
import me.hsgamer.epicmegagames.config.path.ComponentPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;

import java.io.File;
import java.util.List;

public class MessageConfig extends PathableConfig {
    public static final ConfigPath<Component> ERROR_TEMPLATE_NOT_FOUND = new ComponentPath("error.template-not-found", Component.text("&cTemplate not found"));
    public static final ConfigPath<Component> ERROR_ARENA_NOT_FOUND = new ComponentPath("error.arena-not-found", Component.text("&cArena not found"));
    public static final ConfigPath<Component> ERROR_ARENA_NOT_SETUP = new ComponentPath("error.arena-not-setup", Component.text("&cArena not setup"));
    public static final ConfigPath<Component> RESPONSE_MAX_PLAYERS_REACH = new ComponentPath("response.max-players-reach", Component.text("&cMax players reached"));
    public static final ConfigPath<Component> RESPONSE_INCOMPLETE_SETUP = new ComponentPath("response.incomplete-setup", Component.text("&cIncomplete setup"));
    public static final ConfigPath<Component> RESPONSE_NOT_WAITING = new ComponentPath("response.not-waiting", Component.text("&cArena not waiting"));

    public static final ConfigPath<Component> GAME_DUEL_WINNER_MESSAGE = new ComponentPath("game.duel.winner-message", Component.text("&a%player% won the duel!"));
    public static final ConfigPath<Component> GAME_DUEL_NO_WINNER_MESSAGE = new ComponentPath("game.duel.no-winner-message", Component.text("&cNo winner"));
    public static final ConfigPath<Component> GAME_DUEL_BOARD_TITLE = new ComponentPath("game.duel.board.title", Component.text("&e&lDuel"));
    public static final ConfigPath<List<Component>> GAME_DUEL_BOARD_LINES_WAITING = new ComponentListPath("game.duel.board.lines.waiting", List.of(
            Component.text("&eWaiting for players"),
            Component.text("&ePlayers: &a%players%"),
            Component.text("&eTime Left: &a%time%")
    ));
    public static final ConfigPath<List<Component>> GAME_DUEL_BOARD_LINES_INGAME = new ComponentListPath("game.duel.board.lines.ingame", List.of(
            Component.text("&eAlive: &a%players%")
    ));
    public static final ConfigPath<List<Component>> GAME_DUEL_BOARD_LINES_ENDING = new ComponentListPath("game.duel.board.lines.ending", List.of(
            Component.text("&eEnding in: &a%time%"),
            Component.text("&eWinner: &a%winner%")
    ));
    public static final ConfigPath<Component> GAME_DUEL_NOT_ENOUGH_PLAYERS = new ComponentPath("game.duel.not-enough-players", Component.text("&cNot enough players"));

    public MessageConfig() {
        super(new YamlProvider().loadConfiguration(new File("messages.yml")));
    }
}
