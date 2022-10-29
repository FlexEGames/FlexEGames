package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;

import java.time.Duration;

/**
 * The utility class for time
 */
@UtilityClass
public final class TimeUtil {
    /**
     * Format the duration
     *
     * @param duration the duration
     * @return the formatted string
     */
    public static String format(Duration duration) {
        int hours = duration.toHoursPart();
        int minutes = duration.toMinutesPart();
        int seconds = duration.toSecondsPart();

        if (hours <= 0 && minutes <= 0) {
            return Integer.toString(seconds);
        }

        StringBuilder builder = new StringBuilder();
        if (hours > 0) {
            builder.append(String.format("%02d", hours)).append(":");
        }
        builder.append(String.format("%02d", minutes)).append(":");
        builder.append(String.format("%02d", seconds));
        return builder.toString();
    }

    /**
     * Format the duration in milliseconds
     *
     * @param millis the duration in milliseconds
     * @return the formatted string
     */
    public static String format(long millis) {
        return format(Duration.ofMillis(millis));
    }
}
