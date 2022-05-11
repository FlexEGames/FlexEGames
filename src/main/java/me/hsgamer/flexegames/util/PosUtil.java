package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.coordinate.Pos;

import java.util.Optional;

@UtilityClass
public final class PosUtil {
    public static Optional<Pos> convert(String value) {
        String[] split = value.split(",", 5);
        if (split.length != 5) {
            return Optional.empty();
        }
        try {
            return Optional.of(new Pos(
                    Double.parseDouble(split[0].trim()),
                    Double.parseDouble(split[1].trim()),
                    Double.parseDouble(split[2].trim()),
                    Float.parseFloat(split[3].trim()),
                    Float.parseFloat(split[4].trim())
            ));
        } catch (NumberFormatException ignored) {
            return Optional.empty();
        }
    }

    public static String convert(Pos value) {
        return String.format("%s,%s,%s,%s,%s", value.x(), value.y(), value.z(), value.yaw(), value.pitch());
    }
}
