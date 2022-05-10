package me.hsgamer.flexgames.util;

import lombok.experimental.UtilityClass;

import java.time.Duration;

@UtilityClass
public final class TimeUtil {
    public static String format(Duration duration) {
        StringBuilder builder = new StringBuilder();
        int hours = duration.toHoursPart();
        int minutes = duration.toMinutesPart();
        int seconds = duration.toSecondsPart();

        if (hours > 0) {
            builder.append(String.format("%02d", hours)).append(":");
        }
        if (hours > 0 || minutes > 0) {
            builder.append(String.format("%02d", minutes)).append(":");
        }
        builder.append(String.format("%02d", seconds));
        return builder.toString();
    }

    public static String format(long millis) {
        return format(Duration.ofMillis(millis));
    }
}
