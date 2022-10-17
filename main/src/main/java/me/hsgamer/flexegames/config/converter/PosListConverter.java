package me.hsgamer.flexegames.config.converter;

import me.hsgamer.flexegames.util.PosUtil;
import net.minestom.server.coordinate.Pos;

import java.util.List;
import java.util.Optional;

public class PosListConverter extends StringListToObjectConverter<List<Pos>> {
    @Override
    protected List<Pos> deserialize(List<String> rawValue) {
        return rawValue.stream()
                .map(PosUtil::convert)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();
    }

    @Override
    protected List<String> serialize(List<Pos> value) {
        return value.stream().map(PosUtil::convert).toList();
    }
}
