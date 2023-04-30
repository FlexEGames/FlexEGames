package me.hsgamer.flexegames.config;

import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;

/**
 * The message config
 */
public interface MessageConfig {
    @ConfigPath({"error", "game-not-found"})
    default Component getErrorGameNotFound() {
        return Component.text("Game not found").color(NamedTextColor.RED);
    }

    @ConfigPath({"error", "arena-not-found"})
    default Component getErrorArenaNotFound() {
        return Component.text("Arena not found").color(NamedTextColor.RED);
    }

    @ConfigPath({"error", "arena-joined"})
    default Component getErrorArenaJoined() {
        return Component.text("You have already joined the arena").color(NamedTextColor.RED);
    }

    @ConfigPath({"response", "create-arena-successful"})
    default Component getResponseCreateArenaSuccessful() {
        return Component.text("Arena created").color(NamedTextColor.GREEN);
    }

    @ConfigPath({"lobby", "hide-players"})
    default Component getLobbyHidePlayers() {
        return Component.text("You now hide other players").color(NamedTextColor.GREEN);
    }

    @ConfigPath({"lobby", "show-players"})
    default Component getLobbyShowPlayers() {
        return Component.text("You now show other players").color(NamedTextColor.GREEN);
    }

    @ConfigPath({"state", "killing"})
    default Component getStateKilling() {
        return Component.text("Killing");
    }
}
