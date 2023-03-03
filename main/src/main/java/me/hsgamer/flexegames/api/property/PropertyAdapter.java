package me.hsgamer.flexegames.api.property;

import me.hsgamer.hscore.config.annotation.converter.Converter;
import me.hsgamer.hscore.config.annotation.converter.manager.DefaultConverterManager;

/**
 * The adapter to convert the property from the {@link PropertyMap} to the object
 *
 * @param <T> the type of the object
 */
public class PropertyAdapter<T> {
    private final Class<T> type;
    private final Converter converter;

    /**
     * Create a new adapter
     *
     * @param type      the type of the object
     * @param converter the converter
     */
    public PropertyAdapter(Class<T> type, Converter converter) {
        this.type = type;
        this.converter = converter;
    }

    /**
     * Create a new adapter with the default converter
     *
     * @param type the type of the object
     */
    public PropertyAdapter(Class<T> type) {
        this(type, DefaultConverterManager.getConverter(type));
    }

    T get(PropertyMap propertyMap, String key, T defaultValue) {
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

    void set(PropertyMap propertyMap, String key, T value) {
        Object raw = converter.convert(value);
        propertyMap.setProperty(key, raw);
    }
}
