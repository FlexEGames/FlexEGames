package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.timer.TaskSchedule;

@UtilityClass
public final class TaskUtil {
    public static TaskSchedule tick(int tick) {
        return tick <= 0 ? TaskSchedule.nextTick() : TaskSchedule.tick(tick);
    }
}
