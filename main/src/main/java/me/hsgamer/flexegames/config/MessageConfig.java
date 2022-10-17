package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;

public interface MessageConfig {
    @ConfigPath(value = "error.game-not-found", converter = ComponentConverter.class)
    default Component getErrorGameNotFound() {
        return Component.text("Game not found").color(NamedTextColor.RED);
    }

    @ConfigPath(value = "error.arena-not-found", converter = ComponentConverter.class)
    default Component getErrorArenaNotFound() {
        return Component.text("Arena not found").color(NamedTextColor.RED);
    }

    @ConfigPath(value = "error.arena-joined", converter = ComponentConverter.class)
    default Component getErrorArenaJoined() {
        return Component.text("You have already joined the arena").color(NamedTextColor.RED);
    }

    @ConfigPath(value = "response.create-arena-successful", converter = ComponentConverter.class)
    default Component getResponseCreateArenaSuccessful() {
        return Component.text("Arena created").color(NamedTextColor.GREEN);
    }

    @ConfigPath(value = "lobby.hide-players", converter = ComponentConverter.class)
    default Component getLobbyHidePlayers() {
        return Component.text("You now hide other players").color(NamedTextColor.GREEN);
    }

    @ConfigPath(value = "lobby.show-players", converter = ComponentConverter.class)
    default Component getLobbyShowPlayers() {
        return Component.text("You now show other players").color(NamedTextColor.GREEN);
    }
}
