package me.hsgamer.flexegames.template.duel;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.converter.*;
import me.hsgamer.flexegames.util.ChunkLoaderType;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.ItemStack;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DuelGameConfig {
    @ConfigPath(value = "board.title", converter = ComponentConverter.class)
    default Component getBoardTitle() {
        return ComponentConverter.fromString("&6&lDuel");
    }

    @ConfigPath(value = "board.lines.waiting", converter = ComponentListConverter.class)
    default List<Component> getBoardLinesWaiting() {
        return ComponentListConverter.fromStringList(List.of(
                "&eWaiting for players",
                "&ePlayers: &a%players%",
                "&eTime Left: &a%time%"
        ));
    }

    @ConfigPath(value = "board.lines.ingame", converter = ComponentListConverter.class)
    default List<Component> getBoardLinesIngame() {
        return ComponentListConverter.fromStringList(List.of(
                "&eAlive: &a%alive%"
        ));
    }

    @ConfigPath(value = "board.lines.ending", converter = ComponentListConverter.class)
    default List<Component> getBoardLinesEnding() {
        return ComponentListConverter.fromStringList(List.of(
                "&eEnding in: &a%time%",
                "&eWinner: &a%winner%"
        ));
    }

    @ConfigPath(value = "not-enough-players", converter = ComponentConverter.class)
    default Component getNotEnoughPlayers() {
        return ComponentConverter.fromString("&cNot enough players");
    }

    @ConfigPath(value = "winner-message", converter = ComponentConverter.class)
    default Component getWinnerMessage() {
        return ComponentConverter.fromString("&a%winner% &ewon the game");
    }

    @ConfigPath(value = "no-winner-message", converter = ComponentConverter.class)
    default Component getNoWinnerMessage() {
        return ComponentConverter.fromString("&cNo winner");
    }

    @ConfigPath(value = "pos", converter = PosListConverter.class)
    default List<Pos> getPos() {
        return List.of(
                new Pos(-2, 2, 0, -90, 0),
                new Pos(2, 2, 0, 90, 0),
                new Pos(0, 2, -2, 0, 0),
                new Pos(0, 2, 2, 180, 0)
        );
    }

    @ConfigPath(value = "join-pos", converter = PosConverter.class)
    default Pos getJoinPos() {
        return new Pos(0, 2, 0);
    }

    @ConfigPath("max-height")
    default int getMaxHeight() {
        return 2;
    }

    @ConfigPath("waiting-time")
    default int getWaitingTime() {
        return 60;
    }

    @ConfigPath("ending-time")
    default int getEndingTime() {
        return 5;
    }

    @ConfigPath("use-legacy-pvp")
    default boolean isUseLegacyPvp() {
        return false;
    }

    @ConfigPath(value = "kit", converter = NumberObjectMapConverter.class)
    default Map<Number, Map<String, Object>> getKit() {
        return Collections.emptyMap();
    }

    default Map<Integer, ItemStack> getConvertedKit() {
        Map<Integer, ItemStack> kit = new HashMap<>();
        getKit().forEach((key, value) -> kit.put(key.intValue(), ItemBuilder.buildItem(value)));
        return kit;
    }

    @ConfigPath("border-diameter")
    default double getBorderDiameter() {
        return 50.0;
    }

    @ConfigPath("use-world")
    default boolean isUseWorld() {
        return false;
    }

    @ConfigPath(value = "world-loader", converter = ChunkLoaderTypeConverter.class)
    default ChunkLoaderType getWorldLoader() {
        return ChunkLoaderType.ANVIL;
    }

    @ConfigPath("world-name")
    default String getWorldName() {
        return "duel";
    }

    @ConfigPath(value = "chat-format", converter = ComponentConverter.class)
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }
}
