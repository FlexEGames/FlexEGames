package me.hsgamer.flexegames.config.converter;

import me.hsgamer.hscore.config.annotation.converter.Converter;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public abstract class StringObjectMapToObjectConverter<T> implements Converter {
    @Override
    public T convert(Object o) {
        if (o instanceof Map<?, ?> map) {
            Map<String, Object> newMap = new HashMap<>();
            map.forEach((key, value) -> newMap.put(Objects.toString(key), value));
            return deserialize(newMap);
        }
        return null;
    }

    @Override
    public Object convertToRaw(Object o) {
        if (o == null) return null;
        try {
            // noinspection unchecked
            return serialize((T) o);
        } catch (ClassCastException e) {
            return null;
        }
    }

    protected abstract T deserialize(Map<String, Object> rawValue);

    protected abstract Map<String, Object> serialize(T value);
}
