package me.hsgamer.flexegames.config.converter;

import me.hsgamer.flexegames.util.ComponentUtil;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

import java.util.List;

public class ComponentListConverter extends StringListToObjectConverter<List<Component>> {
    @Override
    protected List<Component> deserialize(List<String> rawValue) {
        return ComponentUtil.fromStringList(rawValue);
    }

    @Override
    protected List<String> serialize(List<Component> value) {
        return value.stream()
                .map(LegacyComponentSerializer.legacyAmpersand()::serialize)
                .toList();
    }
}
