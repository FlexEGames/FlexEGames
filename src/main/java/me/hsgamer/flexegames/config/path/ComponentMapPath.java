package me.hsgamer.flexegames.config.path;

import me.hsgamer.hscore.config.path.SerializableMapConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ComponentMapPath extends SerializableMapConfigPath<Map<String, Component>> {
    public ComponentMapPath(String path, Map<String, Component> def) {
        super(path, def);
    }

    @Nullable
    @Override
    public Map<String, Component> convert(@NotNull Map<String, Object> rawValue) {
        Map<String, Component> map = new HashMap<>();
        rawValue.forEach((key, value) -> map.put(key, LegacyComponentSerializer.legacyAmpersand().deserialize(Objects.toString(value, ""))));
        return map;
    }

    @Override
    public @Nullable Map<String, Object> convertToRaw(@NotNull Map<String, Component> value) {
        return value.entrySet().stream()
                .collect(
                        HashMap::new,
                        (map, entry) -> map.put(entry.getKey(), LegacyComponentSerializer.legacyAmpersand().serialize(entry.getValue())),
                        HashMap::putAll
                );
    }
}
