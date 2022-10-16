package me.hsgamer.flexegames.config.converter;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ComponentMapConverter extends StringObjectMapToObjectConverter<Map<String, Component>> {
    @Override
    protected Map<String, Component> deserialize(Map<String, Object> rawValue) {
        Map<String, Component> map = new HashMap<>();
        rawValue.forEach((key, value) -> map.put(key, LegacyComponentSerializer.legacyAmpersand().deserialize(Objects.toString(value, ""))));
        return map;
    }

    @Override
    protected Map<String, Object> serialize(Map<String, Component> value) {
        return value.entrySet().stream()
                .collect(
                        HashMap::new,
                        (map, entry) -> map.put(entry.getKey(), LegacyComponentSerializer.legacyAmpersand().serialize(entry.getValue())),
                        HashMap::putAll
                );
    }
}
