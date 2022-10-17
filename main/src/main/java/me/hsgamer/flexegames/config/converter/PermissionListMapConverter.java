package me.hsgamer.flexegames.config.converter;

import me.hsgamer.hscore.config.annotation.converter.Converter;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public class PermissionListMapConverter implements Converter {
    private final PermissionListConverter permissionListConverter = new PermissionListConverter();

    @Override
    public Object convert(Object rawValue) {
        if (rawValue instanceof Map<?, ?> rawMap) {
            Map<String, Object> map = new LinkedHashMap<>();
            for (var entry : rawMap.entrySet()) {
                String name = Objects.toString(entry.getKey());
                Object value = entry.getValue();
                Object convertedValue = permissionListConverter.convert(value);
                if (convertedValue != null) {
                    map.put(name, convertedValue);
                }
            }
            return map;
        }
        return null;
    }

    @Override
    public Object convertToRaw(Object o) {
        if (o instanceof Map<?, ?> map) {
            Map<String, Object> rawMap = new LinkedHashMap<>();
            for (var entry : map.entrySet()) {
                String name = Objects.toString(entry.getKey());
                Object value = entry.getValue();
                Object convertedValue = permissionListConverter.convertToRaw(value);
                if (convertedValue != null) {
                    rawMap.put(name, convertedValue);
                }
            }
            return rawMap;
        }
        return null;
    }
}
