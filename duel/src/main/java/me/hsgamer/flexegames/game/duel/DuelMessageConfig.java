package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.config.converter.ComponentListConverter;
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

    @ConfigPath("state.waiting")
    default Component getStateWaiting() {
        return Component.text("Waiting");
    }

    @ConfigPath("state.ingame")
    default Component getStateInGame() {
        return Component.text("In Game");
    }

    @ConfigPath("state.ending")
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

    @ConfigPath("winner-message")
    default Component getWinnerMessage() {
        return ComponentConverter.fromString("&a%winner% &ewon the game");
    }

    @ConfigPath("no-winner-message")
    default Component getNoWinnerMessage() {
        return ComponentConverter.fromString("&cNo winner");
    }
}
