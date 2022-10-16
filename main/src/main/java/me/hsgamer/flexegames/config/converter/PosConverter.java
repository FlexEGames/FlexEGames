package me.hsgamer.flexegames.config.converter;

import me.hsgamer.flexegames.util.PosUtil;
import net.minestom.server.coordinate.Pos;

public class PosConverter extends StringToObjectConverter<Pos> {
    @Override
    public Pos deserialize(String rawValue) {
        return PosUtil.convert(rawValue).orElse(null);
    }

    @Override
    public String serialize(Pos value) {
        return PosUtil.convert(value);
    }
}
