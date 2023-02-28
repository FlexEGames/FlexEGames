package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.config.converter.ComponentListConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.Material;

import java.util.List;
import java.util.Map;

public interface PveMessageConfig {
    @ConfigPath("max-players-reached")
    default Component getMaxPlayersReached() {
        return Component.text("The arena is full").color(NamedTextColor.RED);
    }

    @ConfigPath("not-waiting")
    default Component getNotWaiting() {
        return Component.text("The arena is not waiting").color(NamedTextColor.RED);
    }

    @ConfigPath("not-enough-players")
    default Component getNotEnoughPlayers() {
        return Component.text("Not enough players to start the game").color(NamedTextColor.RED);
    }

    @ConfigPath("state.waiting")
    default Component getStateWaiting() {
        return Component.text("Waiting");
    }

    @ConfigPath("state.resting")
    default Component getStateResting() {
        return Component.text("Resting");
    }

    @ConfigPath("state.fighting")
    default Component getStateFighting() {
        return Component.text("Fighting");
    }

    @ConfigPath("state.ending")
    default Component getStateEnding() {
        return Component.text("Ending");
    }


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

    @ConfigPath("chat-format")
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }

    @ConfigPath("start")
    default Component getStartMessage() {
        return Component.text("The game has started!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("stage-start")
    default Component getStageStartMessage() {
        return Component.text("The stage %stage% has started!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("stage-end")
    default Component getStageEndMessage() {
        return Component.text("The stage %stage% has ended!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("end")
    default Component getEndMessage() {
        return Component.text("The game has ended!").color(NamedTextColor.GREEN);
    }

    @ConfigPath("editor.title")
    default Component getEditorTitle() {
        return Component.text("PvE Editor").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
    }

    @ConfigPath("editor.complete")
    default Map<String, Object> getEditorComplete() {
        return Map.of(
                "material", Material.EMERALD.name(),
                "name", "&aComplete"
        );
    }

    @ConfigPath("editor.legacy-pvp")
    default Map<String, Object> getEditorLegacyPvp() {
        return Map.of(
                "material", Material.IRON_SWORD.name(),
                "name", "&aLegacy PVP",
                "lore", List.of(
                        "&7Legacy PVP",
                        "&7",
                        "&7%value%"
                )
        );
    }

    @ConfigPath("editor.tough-mob")
    default Map<String, Object> getEditorToughMob() {
        return Map.of(
                "material", Material.IRON_SWORD.name(),
                "name", "&aTough Mob",
                "lore", List.of(
                        "&7Tough Mob",
                        "&7",
                        "&7%value%"
                )
        );
    }

    @ConfigPath("editor.heal-on-rest")
    default Map<String, Object> getEditorHealOnRest() {
        return Map.of(
                "material", Material.GOLDEN_APPLE.name(),
                "name", "&aHeal on Rest",
                "lore", List.of(
                        "&7Heal on Rest",
                        "&7",
                        "&7%value%"
                )
        );
    }
}
