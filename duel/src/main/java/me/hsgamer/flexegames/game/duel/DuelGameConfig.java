package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.config.converter.ComponentListConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DuelGameConfig {
    @ConfigPath("display-name")
    default Component getDisplayName() {
        return Component.text("Duel").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
    }

    @ConfigPath("description")
    default List<Component> getDescription() {
        return List.of(
                Component.text("Fight with your opponent").color(NamedTextColor.WHITE),
                Component.text("in a small arena").color(NamedTextColor.WHITE),
                Component.text("and be the last one standing").color(NamedTextColor.WHITE)
        );
    }

    @ConfigPath("display-item")
    default Map<String, Object> getDisplayItem() {
        return Map.of(
                "material", Material.WOODEN_SWORD.name(),
                "hide", "all"
        );
    }

    @ConfigPath("arena-display-item")
    default Map<String, Object> getArenaDisplayItem() {
        return Map.of(
                "material", Material.WOODEN_SWORD.name(),
                "name", "%game%",
                "lore", List.of(
                        "&bOwner: &f%owner%",
                        "&bPlayer: &f%players%/%max-players%",
                        "&bTime: &f%time%",
                        "&bStatus: &f%state%"
                ),
                "hide", "all"
        );
    }

    @ConfigPath("board.title")
    default Component getBoardTitle() {
        return ComponentConverter.fromString("&6&lDuel");
    }

    @ConfigPath("board.lines.waiting")
    default List<Component> getBoardLinesWaiting() {
        return ComponentListConverter.fromStringList(List.of(
                "&eWaiting for players",
                "&ePlayers: &a%players%",
                "&eTime Left: &a%time%"
        ));
    }

    @ConfigPath("board.lines.ingame")
    default List<Component> getBoardLinesIngame() {
        return ComponentListConverter.fromStringList(List.of(
                "&eAlive: &a%alive%"
        ));
    }

    @ConfigPath("board.lines.ending")
    default List<Component> getBoardLinesEnding() {
        return ComponentListConverter.fromStringList(List.of(
                "&eEnding in: &a%time%",
                "&eWinner: &a%winner%"
        ));
    }

    @ConfigPath("not-enough-players")
    default Component getNotEnoughPlayers() {
        return ComponentConverter.fromString("&cNot enough players");
    }

    @ConfigPath("winner-message")
    default Component getWinnerMessage() {
        return ComponentConverter.fromString("&a%winner% &ewon the game");
    }

    @ConfigPath("no-winner-message")
    default Component getNoWinnerMessage() {
        return ComponentConverter.fromString("&cNo winner");
    }

    @ConfigPath("pos")
    default List<Pos> getPos() {
        return List.of(
                new Pos(-2, 2, 0, -90, 0),
                new Pos(2, 2, 0, 90, 0),
                new Pos(0, 2, -2, 0, 0),
                new Pos(0, 2, 2, 180, 0)
        );
    }

    @ConfigPath("join-pos")
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

    @ConfigPath("kit")
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

    @ConfigPath("world-loader")
    default String getWorldLoader() {
        return "anvil";
    }

    @ConfigPath("world-name")
    default String getWorldName() {
        return "duel";
    }

    @ConfigPath("chat-format")
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }
}
