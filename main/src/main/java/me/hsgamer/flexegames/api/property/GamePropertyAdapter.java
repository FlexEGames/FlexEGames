package me.hsgamer.flexegames.api.property;

import me.hsgamer.hscore.config.annotation.converter.Converter;
import me.hsgamer.hscore.config.annotation.converter.manager.DefaultConverterManager;

public class GamePropertyAdapter<T> {
    private final Class<T> type;
    private final Converter converter;

    public GamePropertyAdapter(Class<T> type, Converter converter) {
        this.type = type;
        this.converter = converter;
    }

    public GamePropertyAdapter(Class<T> type) {
        this(type, DefaultConverterManager.getConverter(type));
    }

    T get(GamePropertyMap propertyMap, String key, T defaultValue) {
        Object raw = propertyMap.getProperty(key);
        if (raw == null) {
            return defaultValue;
        }
        Object converted = converter.convert(raw);
        if (converted == null) {
            return defaultValue;
        }
        return type.cast(converted);
    }

    void set(GamePropertyMap propertyMap, String key, T value) {
        Object raw = converter.convert(value);
        propertyMap.setProperty(key, raw);
    }
}
