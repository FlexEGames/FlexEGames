package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.coordinate.Pos;

import java.util.Optional;

/**
 * The utility class for {@link Pos}
 */
@UtilityClass
public final class PosUtil {
    /**
     * Convert the string to the {@link Pos}
     *
     * @param value the string
     * @return the {@link Pos}
     */
    public static Optional<Pos> convert(String value) {
        String[] split = value.split(",", 5);
        if (split.length != 3 && split.length != 5) {
            return Optional.empty();
        }
        try {
            return Optional.of(new Pos(
                    Double.parseDouble(split[0].trim()),
                    Double.parseDouble(split[1].trim()),
                    Double.parseDouble(split[2].trim()),
                    split.length > 3 ? Float.parseFloat(split[3].trim()) : 0,
                    split.length > 4 ? Float.parseFloat(split[4].trim()) : 0
            ));
        } catch (NumberFormatException ignored) {
            return Optional.empty();
        }
    }

    /**
     * Convert the {@link Pos} to the string
     *
     * @param value the {@link Pos}
     * @return the string
     */
    public static String convert(Pos value) {
        return String.format("%s,%s,%s,%s,%s", value.x(), value.y(), value.z(), value.yaw(), value.pitch());
    }
}
