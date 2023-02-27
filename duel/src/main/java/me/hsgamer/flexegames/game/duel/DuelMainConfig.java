package me.hsgamer.flexegames.game.duel;

import me.hsgamer.hscore.config.annotation.ConfigPath;

public interface DuelMainConfig {
    @ConfigPath("waiting-time")
    default int getWaitingTime() {
        return 60;
    }

    @ConfigPath("ending-time")
    default int getEndingTime() {
        return 5;
    }
}
