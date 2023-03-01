package me.hsgamer.flexegames.api.property;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class PropertyMap implements Cloneable {
    private final Map<String, Object> serializedMap = new ConcurrentHashMap<>();

    private PropertyMap() {
        // EMPTY
    }

    public static PropertyMap create() {
        return new PropertyMap();
    }

    public static PropertyMap create(Map<String, Object> propertyMap) {
        PropertyMap gameProperties = new PropertyMap();
        gameProperties.serializedMap.putAll(propertyMap);
        return gameProperties;
    }

    void setProperty(String key, Object value) {
        if (value == null) {
            serializedMap.remove(key);
        } else {
            serializedMap.put(key, value);
        }
    }

    Object getProperty(String key) {
        return serializedMap.get(key);
    }

    boolean hasProperty(String key) {
        return serializedMap.containsKey(key);
    }

    public <T> T getProperty(String key, T defaultValue, PropertyAdapter<T> adapter) {
        return adapter.get(this, key, defaultValue);
    }

    public <T> void setProperty(String key, T value, PropertyAdapter<T> adapter) {
        adapter.set(this, key, value);
    }

    public <T> T getProperty(PropertyKeyValue<T> keyValue) {
        return getProperty(keyValue.key(), keyValue.defaultValue(), keyValue.adapter());
    }

    public <T> void setProperty(PropertyKeyValue<T> keyValue, T value) {
        setProperty(keyValue.key(), value, keyValue.adapter());
    }

    public boolean hasProperty(PropertyKeyValue<?> keyValue) {
        return hasProperty(keyValue.key());
    }

    public final Map<String, Object> getSerializedMap() {
        return serializedMap;
    }

    @SuppressWarnings("MethodDoesntCallSuperMethod")
    @Override
    public PropertyMap clone() {
        return create(serializedMap);
    }
}
