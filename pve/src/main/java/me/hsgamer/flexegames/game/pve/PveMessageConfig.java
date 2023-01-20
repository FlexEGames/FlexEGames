package me.hsgamer.flexegames.game.pve;

import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;

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
}
