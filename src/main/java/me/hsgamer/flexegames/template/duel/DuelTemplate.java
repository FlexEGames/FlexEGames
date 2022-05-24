package me.hsgamer.flexegames.template.duel;

import me.hsgamer.flexegames.api.chunk.ChunkLoaderType;
import me.hsgamer.flexegames.api.game.ArenaGame;
import me.hsgamer.flexegames.api.game.Template;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.config.path.*;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.MapConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DuelTemplate implements Template {
    private static final ConfigPath<Component> boardTitlePath = new ComponentPath("board.title", MessageConfig.GAME_DUEL_BOARD_TITLE.getValue());
    private static final ConfigPath<List<Component>> boardLinesWaitingPath = new ComponentListPath("board.lines.waiting", MessageConfig.GAME_DUEL_BOARD_LINES_WAITING.getValue());
    private static final ConfigPath<List<Component>> boardLinesIngamePath = new ComponentListPath("board.lines.ingame", MessageConfig.GAME_DUEL_BOARD_LINES_INGAME.getValue());
    private static final ConfigPath<List<Component>> boardLinesEndingPath = new ComponentListPath("board.lines.ending", MessageConfig.GAME_DUEL_BOARD_LINES_ENDING.getValue());
    private static final ConfigPath<Component> winnerMessagePath = new ComponentPath("winner-message", MessageConfig.GAME_DUEL_WINNER_MESSAGE.getValue());
    private static final ConfigPath<Component> noWinnerMessagePath = new ComponentPath("no-winner-message", MessageConfig.GAME_DUEL_NO_WINNER_MESSAGE.getValue());
    private static final ConfigPath<Component> notEnoughPlayersPath = new ComponentPath("not-enough-players", MessageConfig.GAME_DUEL_NOT_ENOUGH_PLAYERS.getValue());
    private static final ConfigPath<Component> displayNamePath = new ComponentPath("display-name", "&e&lDuel");
    private static final ConfigPath<List<Component>> descriptionPath = new ComponentListPath("description", Collections.singletonList(
            Component.text("Kill other players to win").color(NamedTextColor.WHITE)
    ));
    private static final MapConfigPath displayItemPath = new MapConfigPath("display-item", Map.of(
            "material", Material.DIAMOND_SWORD.name(),
            "hide", "all"
    ));
    private static final MapConfigPath gameDisplayItemPath = new MapConfigPath("game-display-item", Map.of(
            "material", Material.DIAMOND_SWORD.name(),
            "name", "%template%",
            "lore", List.of(
                    "&ePlayers: &f%players%/%max-players%",
                    "&eState: &f%state%",
                    "&eTime: &f%time%"
            ),
            "hide", "all"
    ));
    private static final ConfigPath<List<Pos>> posPath = new PosListPath("pos", List.of(
            new Pos(-2, 2, 0, -90, 0),
            new Pos(2, 2, 0, 90, 0),
            new Pos(0, 2, -2, 0, 0),
            new Pos(0, 2, 2, 180, 0)
    ));
    private static final ConfigPath<Pos> joinPosPath = new PosPath("join-pos", new Pos(0, 2, 0));
    private static final ConfigPath<Integer> maxHeightPath = Paths.integerPath("max-height", 2);
    private static final ConfigPath<Integer> waitingTimePath = Paths.integerPath("waiting-time", 60);
    private static final ConfigPath<Integer> endingTimePath = Paths.integerPath("ending-time", 5);
    private static final ConfigPath<Boolean> useLegacyPvpPath = Paths.booleanPath("use-legacy-pvp", false);
    private static final NumberObjectMapPath kitPath = new NumberObjectMapPath("kit", Collections.emptyMap());
    private static final ConfigPath<Double> borderDiameterPath = Paths.doublePath("border-diameter", 50.0);
    private static final ConfigPath<Boolean> useWorldPath = Paths.booleanPath("use-world", false);
    private static final ConfigPath<ChunkLoaderType> worldLoaderPath = new ChunkLoaderTypePath("world-loader", ChunkLoaderType.ANVIL);
    private static final ConfigPath<String> worldNamePath = Paths.stringPath("world-name", "duel");
    final Component displayName;
    final List<Component> description;
    final Map<String, Object> displayItem;
    final Map<String, Object> gameDisplayItem;
    final List<Pos> posList;
    final Pos joinPos;
    final int maxHeight;
    final int waitingTime;
    final int endingTime;
    final boolean useLegacyPvp;
    final Map<Integer, ItemStack> kit;
    final double borderDiameter;
    final boolean useWorld;
    final ChunkLoaderType worldLoader;
    final String worldName;
    final Component winnerMessage;
    final Component noWinnerMessage;
    final Component boardTitle;
    final List<Component> boardLinesWaiting;
    final List<Component> boardLinesIngame;
    final List<Component> boardLinesEnding;
    final Component notEnoughPlayers;

    public DuelTemplate(Config config) {
        displayName = displayNamePath.getValue(config);
        description = descriptionPath.getValue(config);
        displayItem = displayItemPath.getValue(config);
        gameDisplayItem = gameDisplayItemPath.getValue(config);
        posList = posPath.getValue(config);
        joinPos = joinPosPath.getValue(config);
        maxHeight = maxHeightPath.getValue(config);
        waitingTime = waitingTimePath.getValue(config);
        endingTime = endingTimePath.getValue(config);
        useLegacyPvp = useLegacyPvpPath.getValue(config);
        kit = new HashMap<>();
        kitPath.getValue(config).forEach((key, value) -> kit.put(key.intValue(), ItemBuilder.buildItem(value)));
        borderDiameter = borderDiameterPath.getValue(config);
        useWorld = useWorldPath.getValue(config);
        worldLoader = worldLoaderPath.getValue(config);
        worldName = worldNamePath.getValue(config);
        winnerMessage = winnerMessagePath.getValue(config);
        noWinnerMessage = noWinnerMessagePath.getValue(config);
        boardTitle = boardTitlePath.getValue(config);
        boardLinesWaiting = boardLinesWaitingPath.getValue(config);
        boardLinesIngame = boardLinesIngamePath.getValue(config);
        boardLinesEnding = boardLinesEndingPath.getValue(config);
        notEnoughPlayers = notEnoughPlayersPath.getValue(config);
    }

    @Override
    public ArenaGame createGame(Arena arena) {
        return new DuelGame(this, arena);
    }

    @Override
    public Component getDisplayName() {
        return displayName;
    }

    @Override
    public List<Component> getDescription() {
        return description;
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemBuilder.buildItem(displayItem).withDisplayName(displayName).withLore(description);
    }
}
