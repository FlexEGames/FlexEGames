package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;

public interface PveMessageConfig {
    @ConfigPath(value = "max-players-reached", converter = ComponentConverter.class)
    default Component getMaxPlayersReached() {
        return Component.text("The arena is full").color(NamedTextColor.RED);
    }

    @ConfigPath(value = "not-waiting", converter = ComponentConverter.class)
    default Component getNotWaiting() {
        return Component.text("The arena is not waiting").color(NamedTextColor.RED);
    }

    @ConfigPath(value = "not-enough-players", converter = ComponentConverter.class)
    default Component getNotEnoughPlayers() {
        return Component.text("Not enough players to start the game").color(NamedTextColor.RED);
    }

    @ConfigPath(value = "state.waiting", converter = ComponentConverter.class)
    default Component getStateWaiting() {
        return Component.text("Waiting");
    }

    @ConfigPath(value = "state.resting", converter = ComponentConverter.class)
    default Component getStateResting() {
        return Component.text("Resting");
    }

    @ConfigPath(value = "state.fighting", converter = ComponentConverter.class)
    default Component getStateFighting() {
        return Component.text("Fighting");
    }

    @ConfigPath(value = "state.ending", converter = ComponentConverter.class)
    default Component getStateEnding() {
        return Component.text("Ending");
    }

    @ConfigPath(value = "state.killing", converter = ComponentConverter.class)
    default Component getStateKilling() {
        return Component.text("Killing");
    }
}
