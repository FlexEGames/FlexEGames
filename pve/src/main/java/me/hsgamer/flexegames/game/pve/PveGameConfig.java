package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.config.converter.ComponentListConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface PveGameConfig {
    @ConfigPath("display-name")
    default Component getDisplayName() {
        return Component.text("PvE").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
    }

    @ConfigPath("description")
    default List<Component> getDescription() {
        return List.of(
                Component.text("Kill the mobs to survive!").color(NamedTextColor.WHITE),
                Component.text("The last one standing wins!").color(NamedTextColor.WHITE)
        );
    }

    @ConfigPath("display-item")
    default Map<String, Object> getDisplayItem() {
        return Map.of(
                "material", Material.ZOMBIE_HEAD.name(),
                "hide", "all"
        );
    }

    @ConfigPath("arena-display-item")
    default Map<String, Object> getArenaDisplayItem() {
        return Map.of(
                "material", Material.ZOMBIE_HEAD.name(),
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
        return ComponentConverter.fromString("&6&lPvE");
    }

    @ConfigPath("board.lines.waiting")
    default List<Component> getBoardLinesWaiting() {
        return ComponentListConverter.fromStringList(List.of(
                "&eWaiting for players",
                "&ePlayers: &a%players%/%max-players%",
                "&eTime Left: &a%time%"
        ));
    }

    @ConfigPath("board.lines.resting")
    default List<Component> getBoardLinesResting() {
        return ComponentListConverter.fromStringList(List.of(
                "&eResting...",
                "&eStage: &a%stage%",
                "&eAlive: &a%alive%",
                "&eTime Left: &a%time%"
        ));
    }

    @ConfigPath("board.lines.fighting")
    default List<Component> getBoardLinesFighting() {
        return ComponentListConverter.fromStringList(List.of(
                "&eFighting...",
                "&eStage: &a%stage%",
                "&eAlive: &a%alive%",
                "&eMob: &a%mob%",
                "&eTime Left: &a%time%"
        ));
    }

    @ConfigPath("board.lines.ending")
    default List<Component> getBoardLinesEnding() {
        return ComponentListConverter.fromStringList(List.of(
                "&eGame Over",
                "&eStage: &a%stage%",
                "&eEnding in: &a%time%"
        ));
    }

    @ConfigPath("use-legacy-pvp")
    default boolean isUseLegacyPvp() {
        return false;
    }

    @ConfigPath("tough-mob")
    default boolean isToughMob() {
        return false;
    }

    @ConfigPath("mayhem")
    default boolean isMayhem() {
        return false;
    }

    @ConfigPath("heal-on-rest")
    default boolean isHealOnRest() {
        return true;
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

    @ConfigPath("kit")
    default Map<Number, Map<String, Object>> getKit() {
        return Collections.emptyMap();
    }

    default Map<Integer, ItemStack> getConvertedKit() {
        Map<Integer, ItemStack> kit = new HashMap<>();
        getKit().forEach((key, value) -> kit.put(key.intValue(), ItemBuilder.buildItem(value)));
        return kit;
    }

    @ConfigPath("chat-format")
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }

    @ConfigPath("message.start")
    default Component getStartMessage() {
        return Component.text("The game has started!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("message.stage-start")
    default Component getStageStartMessage() {
        return Component.text("The stage %stage% has started!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("message.stage-end")
    default Component getStageEndMessage() {
        return Component.text("The stage %stage% has ended!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("message.end")
    default Component getEndMessage() {
        return Component.text("The game has ended!").color(NamedTextColor.GREEN);
    }
}
