package me.hsgamer.flexegames.game.pve;

import me.hsgamer.hscore.config.annotation.ConfigPath;

public interface PveMainConfig {
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
}
