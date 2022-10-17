package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.config.converter.NumberObjectMapConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.minestom.server.item.ItemStack;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public interface PveGameConfig {
    @ConfigPath("tough-mob")
    default boolean isToughMob() {
        return false;
    }

    @ConfigPath("mayhem")
    default boolean isMayhem() {
        return false;
    }

    @ConfigPath("use-legacy-pvp")
    default boolean isUseLegacyPvp() {
        return false;
    }

    @ConfigPath("max-players")
    default int getMaxPlayers() {
        return 10;
    }

    @ConfigPath("min-players")
    default int getMinPlayers() {
        return 1;
    }

    @ConfigPath("waiting-time")
    default int getWaitingTime() {
        return 60;
    }

    @ConfigPath("resting-time")
    default int getRestingTime() {
        return 30;
    }

    @ConfigPath("ending-time")
    default int getEndingTime() {
        return 10;
    }

    @ConfigPath("spawn-delay")
    default int getSpawnDelay() {
        return 500;
    }

    @ConfigPath("mob-per-spawn")
    default int getMobPerSpawn() {
        return 5;
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

    @ConfigPath(value = "chat-format", converter = ComponentConverter.class)
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }

    @ConfigPath(value = "message.start", converter = ComponentConverter.class)
    default Component getStartMessage() {
        return Component.text("The game has started!").color(NamedTextColor.GREEN);
    }

    @ConfigPath(value = "message.stage-start", converter = ComponentConverter.class)
    default Component getStageStartMessage() {
        return Component.text("The stage %stage% has started!").color(NamedTextColor.GREEN);
    }

    @ConfigPath(value = "message.stage-end", converter = ComponentConverter.class)
    default Component getStageEndMessage() {
        return Component.text("The stage %stage% has ended!").color(NamedTextColor.GREEN);
    }

    @ConfigPath(value = "message.end", converter = ComponentConverter.class)
    default Component getEndMessage() {
        return Component.text("The game has ended!").color(NamedTextColor.GREEN);
    }
}