package me.hsgamer.flexgames.config.path;

import me.hsgamer.hscore.config.path.SerializableMapConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;

public class MapPath extends SerializableMapConfigPath<Map<String, Object>> {
    public MapPath(String path, Map<String, Object> def) {
        super(path, def);
    }

    @Override
    public @Nullable Map<String, Object> convert(@NotNull Map<String, Object> rawValue) {
        return rawValue.isEmpty() ? null : rawValue;
    }

    @Override
    public @Nullable Map<String, Object> convertToRaw(@NotNull Map<String, Object> value) {
        return value;
    }
}
