package me.hsgamer.flexegames.config.converter;

import me.hsgamer.hscore.config.annotation.converter.Converter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class MapListConverter implements Converter {
    @Override
    public Object convert(Object rawValue) {
        if (rawValue instanceof List<?> rawList) {
            List<Map<String, Object>> list = new ArrayList<>();
            for (Object o : rawList) {
                if (o instanceof Map) {
                    Map<String, Object> map = new LinkedHashMap<>();
                    for (Map.Entry<?, ?> entry : ((Map<?, ?>) o).entrySet()) {
                        map.put(entry.getKey().toString(), entry.getValue());
                    }
                    list.add(map);
                }
            }
            return list;
        } else if (rawValue instanceof Map<?, ?> rawMap) {
            Map<String, Object> map = new LinkedHashMap<>();
            for (Map.Entry<?, ?> entry : rawMap.entrySet()) {
                map.put(entry.getKey().toString(), entry.getValue());
            }
            return List.of(map);
        }
        return null;
    }

    @Override
    public Object convertToRaw(Object o) {
        return o;
    }
}
