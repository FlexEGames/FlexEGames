package me.hsgamer.flexegames.template.duel;

import me.hsgamer.flexegames.config.YamlPathableConfig;
import me.hsgamer.flexegames.config.path.ComponentPath;
import me.hsgamer.flexegames.config.path.StringComponentListPath;
import me.hsgamer.hscore.config.path.ConfigPath;
import net.kyori.adventure.text.Component;

import java.util.List;

public class DuelMessageConfig extends YamlPathableConfig {
    public static final ConfigPath<Component> BOARD_TITLE = new ComponentPath("board.title", "&e&lDuel");
    public static final ConfigPath<List<Component>> BOARD_LINES_WAITING = new StringComponentListPath("board.lines.waiting", List.of(
            "&eWaiting for players",
            "&ePlayers: &a%players%",
            "&eTime Left: &a%time%"
    ));
    public static final ConfigPath<List<Component>> BOARD_LINES_INGAME = new StringComponentListPath("board.lines.ingame", List.of(
            "&eAlive: &a%alive%"
    ));
    public static final ConfigPath<List<Component>> BOARD_LINES_ENDING = new StringComponentListPath("board.lines.ending", List.of(
            "&eEnding in: &a%time%",
            "&eWinner: &a%winner%"
    ));
    public static final ConfigPath<Component> NOT_ENOUGH_PLAYERS = new ComponentPath("not-enough-players", "&cNot enough players");
    public static final ConfigPath<Component> WINNER_MESSAGE = new ComponentPath("winner-message", "&a%winner% won the duel!");
    public static final ConfigPath<Component> NO_WINNER_MESSAGE = new ComponentPath("no-winner-message", "&cNo winner");

    public DuelMessageConfig(DuelExtension extension) {
        super(extension.getDataDirectory().resolve("messages.yml").toFile());
    }
}
