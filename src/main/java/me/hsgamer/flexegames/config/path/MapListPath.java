package me.hsgamer.flexegames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class MapListPath extends AdvancedConfigPath<Object, List<Map<String, Object>>> {
    public MapListPath(@NotNull String path, @Nullable List<Map<String, Object>> def) {
        super(path, def);
    }

    @Override
    public @Nullable Object getFromConfig(@NotNull Config config) {
        return config.get(getPath());
    }

    @Override
    public @Nullable List<Map<String, Object>> convert(@NotNull Object rawValue) {
        if (rawValue instanceof List) {
            List<Map<String, Object>> list = new ArrayList<>();
            for (Object o : (List<?>) rawValue) {
                if (o instanceof Map) {
                    Map<String, Object> map = new LinkedHashMap<>();
                    for (Map.Entry<?, ?> entry : ((Map<?, ?>) o).entrySet()) {
                        map.put(entry.getKey().toString(), entry.getValue());
                    }
                    list.add(map);
                }
            }
            return list;
        } else if (rawValue instanceof Map) {
            Map<String, Object> map = new LinkedHashMap<>();
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) rawValue).entrySet()) {
                map.put(entry.getKey().toString(), entry.getValue());
            }
            return List.of(map);
        }
        return null;
    }

    @Override
    public @Nullable Object convertToRaw(@NotNull List<Map<String, Object>> value) {
        return value;
    }
}
