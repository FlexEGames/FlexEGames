package me.hsgamer.flexegames.config.path;

import me.hsgamer.hscore.config.path.SerializableMapConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class NumberObjectMapPath extends SerializableMapConfigPath<Map<Number, Map<String, Object>>> {
    public NumberObjectMapPath(@NotNull String path, @Nullable Map<Number, Map<String, Object>> def) {
        super(path, def);
    }

    @Override
    public @Nullable Map<Number, Map<String, Object>> convert(@NotNull Map<String, Object> rawValue) {
        return rawValue.entrySet().stream()
                .collect(Collectors.toMap(
                        entry -> Double.parseDouble(entry.getKey()),
                        entry -> {
                            Map<String, Object> value = new LinkedHashMap<>();
                            if (entry.getValue() instanceof Map) {
                                ((Map<?, ?>) entry.getValue()).forEach((key, value1) -> value.put(key.toString(), value1));
                            }
                            return value;
                        },
                        (a, b) -> b,
                        LinkedHashMap::new
                ));
    }

    @Override
    public @Nullable Map<String, Object> convertToRaw(@NotNull Map<Number, Map<String, Object>> value) {
        Map<String, Object> map = new LinkedHashMap<>();
        for (Map.Entry<Number, Map<String, Object>> entry : value.entrySet()) {
            map.put(Objects.toString(entry.getKey()), entry.getValue());
        }
        return map;
    }
}
