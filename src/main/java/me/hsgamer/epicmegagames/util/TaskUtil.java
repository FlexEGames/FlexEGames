package me.hsgamer.epicmegagames.util;

import net.minestom.server.timer.TaskSchedule;

public final class TaskUtil {
    public static TaskSchedule tick(int tick) {
        return tick <= 0 ? TaskSchedule.nextTick() : TaskSchedule.tick(tick);
    }
}
