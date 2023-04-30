package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.util.ComponentUtil;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.Material;

import java.util.List;
import java.util.Map;

public interface DuelMessageConfig {
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

    @ConfigPath({"state", "waiting"})
    default Component getStateWaiting() {
        return Component.text("Waiting");
    }

    @ConfigPath({"state", "ingame"})
    default Component getStateInGame() {
        return Component.text("In Game");
    }

    @ConfigPath({"state", "ending"})
    default Component getStateEnding() {
        return Component.text("Ending");
    }

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
                        ComponentUtil.toString(Component.text("Owner: ").color(NamedTextColor.BLUE).append(Component.text("%owner%").color(NamedTextColor.WHITE))),
                        ComponentUtil.toString(Component.text("Player: ").color(NamedTextColor.BLUE).append(Component.text("%players%/%max-players%").color(NamedTextColor.WHITE))),
                        ComponentUtil.toString(Component.text("Time: ").color(NamedTextColor.BLUE).append(Component.text("%time%").color(NamedTextColor.WHITE))),
                        ComponentUtil.toString(Component.text("Status: ").color(NamedTextColor.BLUE).append(Component.text("%state%").color(NamedTextColor.WHITE)))
                ),
                "hide", "all"
        );
    }

    @ConfigPath({"board", "title"})
    default Component getBoardTitle() {
        return Component.text("Duel").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
    }

    @ConfigPath({"board", "lines", "waiting"})
    default List<Component> getBoardLinesWaiting() {
        return List.of(
                Component.text("Waiting for players").color(NamedTextColor.YELLOW),
                Component.text("Players: ").color(NamedTextColor.YELLOW).append(Component.text("%players%").color(NamedTextColor.WHITE)),
                Component.text("Time Left: ").color(NamedTextColor.YELLOW).append(Component.text("%time%").color(NamedTextColor.WHITE))
        );
    }

    @ConfigPath({"board", "lines", "ingame"})
    default List<Component> getBoardLinesIngame() {
        return List.of(
                Component.text("Alive: ").color(NamedTextColor.YELLOW).append(Component.text("%alive%").color(NamedTextColor.WHITE))
        );
    }

    @ConfigPath({"board", "lines", "ending"})
    default List<Component> getBoardLinesEnding() {
        return List.of(
                Component.text("Ending in: ").color(NamedTextColor.YELLOW).append(Component.text("%time%").color(NamedTextColor.WHITE)),
                Component.text("Winner: ").color(NamedTextColor.YELLOW).append(Component.text("%winner%").color(NamedTextColor.WHITE))
        );
    }

    @ConfigPath("winner-message")
    default Component getWinnerMessage() {
        return Component.empty()
                .append(Component.text("%winner%").color(NamedTextColor.GREEN))
                .append(Component.text(" won the game").color(NamedTextColor.YELLOW));
    }

    @ConfigPath("no-winner-message")
    default Component getNoWinnerMessage() {
        return Component.text("No winner").color(NamedTextColor.RED);
    }

    @ConfigPath("chat-format")
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }

    @ConfigPath({"editor", "title"})
    default Component getEditorTitle() {
        return Component.text("Duel Editor").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
    }

    @ConfigPath({"editor", "complete"})
    default Map<String, Object> getEditorComplete() {
        return Map.of(
                "material", Material.EMERALD.name(),
                "name", ComponentUtil.toString(Component.text("Complete").color(NamedTextColor.GREEN).decorate(TextDecoration.BOLD))
        );
    }

    @ConfigPath({"editor", "legacy-pvp"})
    default Map<String, Object> getEditorLegacyPvp() {
        return Map.of(
                "material", Material.IRON_SWORD.name(),
                "hide", "all",
                "name", ComponentUtil.toString(Component.text("Legacy PVP").color(NamedTextColor.GREEN).decorate(TextDecoration.BOLD)),
                "lore", List.of(
                        ComponentUtil.toString(Component.text("Enable this to use 1.8 PVP mechanics").color(NamedTextColor.WHITE)),
                        "",
                        ComponentUtil.toString(Component.text("Current: ").color(NamedTextColor.WHITE).append(Component.text("%value%").color(NamedTextColor.GREEN)))
                )
        );
    }
}
