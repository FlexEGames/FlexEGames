package me.hsgamer.epicmegagames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class NumberObjectMapPath extends AdvancedConfigPath<Map<String, Map<String, Object>>, Map<Number, Map<String, Object>>> {
    public NumberObjectMapPath(@NotNull String path, @Nullable Map<Number, Map<String, Object>> def) {
        super(path, def);
    }

    @Override
    public @Nullable Map<String, Map<String, Object>> getFromConfig(@NotNull Config config) {
        Map<String, Object> rawMap = config.getNormalizedValues(getPath(), false);
        Map<String, Map<String, Object>> map = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : rawMap.entrySet()) {
            Object value = entry.getValue();
            if (value instanceof Map) {
                Map<String, Object> mapValue = new LinkedHashMap<>();
                for (Map.Entry<?, ?> entry1 : ((Map<?, ?>) value).entrySet()) {
                    mapValue.put(Objects.toString(entry1.getKey()), entry1.getValue());
                }
                map.put(entry.getKey(), mapValue);
            }
        }
        return map.isEmpty() ? null : map;
    }

    @Override
    public @Nullable Map<Number, Map<String, Object>> convert(@NotNull Map<String, Map<String, Object>> rawValue) {
        return rawValue.entrySet().stream()
                .collect(Collectors.toMap(
                        entry -> Double.parseDouble(entry.getKey()),
                        Map.Entry::getValue,
                        (a, b) -> b,
                        LinkedHashMap::new
                ));
    }

    @Override
    public @Nullable Map<String, Map<String, Object>> convertToRaw(@NotNull Map<Number, Map<String, Object>> value) {
        Map<String, Map<String, Object>> map = new LinkedHashMap<>();
        for (Map.Entry<Number, Map<String, Object>> entry : value.entrySet()) {
            map.put(Objects.toString(entry.getKey()), entry.getValue());
        }
        return map;
    }
}
