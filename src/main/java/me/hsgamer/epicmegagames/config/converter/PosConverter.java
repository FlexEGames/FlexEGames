package me.hsgamer.epicmegagames.config.converter;

import me.hsgamer.hscore.config.annotation.converter.Converter;
import net.minestom.server.coordinate.Pos;

import java.util.Objects;

public class PosConverter implements Converter {
    @Override
    public Object convert(Object raw) {
        String str = Objects.toString(raw, "");
        String[] split = str.split(",", 5);
        if (split.length != 5) {
            return new Pos(0, 0, 0);
        }
        try {
            return new Pos(
                    Double.parseDouble(split[0].trim()),
                    Double.parseDouble(split[1].trim()),
                    Double.parseDouble(split[2].trim()),
                    Float.parseFloat(split[3].trim()),
                    Float.parseFloat(split[4].trim())
            );
        } catch (NumberFormatException ignored) {
            return new Pos(0, 0, 0);
        }
    }

    @Override
    public Object convertToRaw(Object value) {
        Pos pos = (Pos) value;
        return String.format("%s,%s,%s,%s,%s", pos.x(), pos.y(), pos.z(), pos.yaw(), pos.pitch());
    }
}
