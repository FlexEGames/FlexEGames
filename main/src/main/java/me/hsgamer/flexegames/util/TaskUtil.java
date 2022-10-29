package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.timer.TaskSchedule;

/**
 * Utilities for {@link net.minestom.server.timer.Task} and {@link net.minestom.server.timer.TaskSchedule}
 */
@UtilityClass
public final class TaskUtil {
    /**
     * Get the tick schedule
     *
     * @param tick the tick
     * @return the schedule
     */
    public static TaskSchedule tick(int tick) {
        return tick <= 0 ? TaskSchedule.nextTick() : TaskSchedule.tick(tick);
    }
}
