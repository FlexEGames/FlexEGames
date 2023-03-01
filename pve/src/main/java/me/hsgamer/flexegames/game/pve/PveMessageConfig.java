package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
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
                        ComponentConverter.toString(Component.text("Owner: ").color(NamedTextColor.BLUE).append(Component.text("%owner%").color(NamedTextColor.WHITE))),
                        ComponentConverter.toString(Component.text("Player: ").color(NamedTextColor.BLUE).append(Component.text("%players%/%max-players%").color(NamedTextColor.WHITE))),
                        ComponentConverter.toString(Component.text("Time: ").color(NamedTextColor.BLUE).append(Component.text("%time%").color(NamedTextColor.WHITE))),
                        ComponentConverter.toString(Component.text("Status: ").color(NamedTextColor.BLUE).append(Component.text("%state%").color(NamedTextColor.WHITE)))
                ),
                "hide", "all"
        );
    }

    @ConfigPath("board.title")
    default Component getBoardTitle() {
        return Component.text("PvE").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
    }

    @ConfigPath("board.lines.waiting")
    default List<Component> getBoardLinesWaiting() {
        return List.of(
                Component.text("Waiting for players").color(NamedTextColor.YELLOW),
                Component.text("Players: ").color(NamedTextColor.YELLOW).append(Component.text("%players%/%max-players%").color(NamedTextColor.GREEN)),
                Component.text("Time Left: ").color(NamedTextColor.YELLOW).append(Component.text("%time%").color(NamedTextColor.GREEN))
        );
    }

    @ConfigPath("board.lines.resting")
    default List<Component> getBoardLinesResting() {
        return List.of(
                Component.text("Resting...").color(NamedTextColor.YELLOW),
                Component.text("Stage: ").color(NamedTextColor.YELLOW).append(Component.text("%stage%").color(NamedTextColor.GREEN)),
                Component.text("Alive: ").color(NamedTextColor.YELLOW).append(Component.text("%alive%").color(NamedTextColor.GREEN)),
                Component.text("Time Left: ").color(NamedTextColor.YELLOW).append(Component.text("%time%").color(NamedTextColor.GREEN))
        );
    }

    @ConfigPath("board.lines.fighting")
    default List<Component> getBoardLinesFighting() {
        return List.of(
                Component.text("Fighting...").color(NamedTextColor.YELLOW),
                Component.text("Stage: ").color(NamedTextColor.YELLOW).append(Component.text("%stage%").color(NamedTextColor.GREEN)),
                Component.text("Alive: ").color(NamedTextColor.YELLOW).append(Component.text("%alive%").color(NamedTextColor.GREEN)),
                Component.text("Mob: ").color(NamedTextColor.YELLOW).append(Component.text("%mob%").color(NamedTextColor.GREEN)),
                Component.text("Time Left: ").color(NamedTextColor.YELLOW).append(Component.text("%time%").color(NamedTextColor.GREEN))
        );
    }

    @ConfigPath("board.lines.ending")
    default List<Component> getBoardLinesEnding() {
        return List.of(
                Component.text("Game Over").color(NamedTextColor.YELLOW),
                Component.text("Stage: ").color(NamedTextColor.YELLOW).append(Component.text("%stage%").color(NamedTextColor.GREEN)),
                Component.text("Ending in: ").color(NamedTextColor.YELLOW).append(Component.text("%time%").color(NamedTextColor.GREEN))
        );
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
                "name", ComponentConverter.toString(Component.text("Complete").color(NamedTextColor.GREEN).decorate(TextDecoration.BOLD))
        );
    }

    @ConfigPath("editor.legacy-pvp")
    default Map<String, Object> getEditorLegacyPvp() {
        return Map.of(
                "material", Material.IRON_SWORD.name(),
                "name", ComponentConverter.toString(Component.text("Legacy PVP").color(NamedTextColor.GREEN).decorate(TextDecoration.BOLD)),
                "lore", List.of(
                        ComponentConverter.toString(Component.text("Enable this to use 1.8 PVP mechanics").color(NamedTextColor.WHITE)),
                        "",
                        ComponentConverter.toString(Component.text("Current: ").color(NamedTextColor.WHITE).append(Component.text("%value%").color(NamedTextColor.GREEN)))
                )
        );
    }

    @ConfigPath("editor.tough-mob")
    default Map<String, Object> getEditorToughMob() {
        return Map.of(
                "material", Material.IRON_SWORD.name(),
                "name", ComponentConverter.toString(Component.text("Tough Mob").color(NamedTextColor.GREEN).decorate(TextDecoration.BOLD)),
                "lore", List.of(
                        ComponentConverter.toString(Component.text("Enable this to make mobs tougher").color(NamedTextColor.WHITE)),
                        "",
                        ComponentConverter.toString(Component.text("Current: ").color(NamedTextColor.WHITE).append(Component.text("%value%").color(NamedTextColor.GREEN)))
                )
        );
    }

    @ConfigPath("editor.heal-on-rest")
    default Map<String, Object> getEditorHealOnRest() {
        return Map.of(
                "material", Material.GOLDEN_APPLE.name(),
                "name", ComponentConverter.toString(Component.text("Heal on Rest").color(NamedTextColor.GREEN).decorate(TextDecoration.BOLD)),
                "lore", List.of(
                        ComponentConverter.toString(Component.text("Enable this to heal players after each stage").color(NamedTextColor.WHITE)),
                        "",
                        ComponentConverter.toString(Component.text("Current: ").color(NamedTextColor.WHITE).append(Component.text("%value%").color(NamedTextColor.GREEN)))
                )
        );
    }
}
