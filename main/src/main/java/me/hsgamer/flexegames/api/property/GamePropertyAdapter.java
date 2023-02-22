package me.hsgamer.flexegames.api.property;

import me.hsgamer.hscore.config.annotation.converter.Converter;
import me.hsgamer.hscore.config.annotation.converter.manager.DefaultConverterManager;

public class GamePropertyAdapter<T> {
    private final Class<T> type;
    private final Converter converter;
    private final T defaultValue;

    public GamePropertyAdapter(Class<T> type, Converter converter, T defaultValue) {
        this.type = type;
        this.converter = converter;
        this.defaultValue = defaultValue;
    }

    public GamePropertyAdapter(Class<T> type, Converter converter) {
        this(type, converter, null);
    }

    public GamePropertyAdapter(Class<T> type, T defaultValue) {
        this(type, DefaultConverterManager.getConverter(type), defaultValue);
    }

    public GamePropertyAdapter(Class<T> type) {
        this(type, DefaultConverterManager.getConverter(type), null);
    }

    public T get(GamePropertyMap propertyMap, String key) {
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

    public void set(GamePropertyMap propertyMap, String key, T value) {
        Object raw = converter.convert(value);
        propertyMap.setProperty(key, raw);
    }
}
