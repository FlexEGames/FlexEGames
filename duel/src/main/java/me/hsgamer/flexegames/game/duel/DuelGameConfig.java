package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.builder.ItemBuilder;
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
