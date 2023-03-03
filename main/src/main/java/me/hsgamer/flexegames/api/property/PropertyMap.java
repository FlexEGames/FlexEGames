package me.hsgamer.flexegames.api.property;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The property map for the game
 */
public class PropertyMap implements Cloneable {
    private final Map<String, Object> serializedMap = new ConcurrentHashMap<>();

    private PropertyMap() {
        // EMPTY
    }

    /**
     * Create a new instance
     *
     * @return the instance
     */
    public static PropertyMap create() {
        return new PropertyMap();
    }

    /**
     * Create a new instance from a map
     *
     * @param propertyMap the map
     * @return the instance
     */
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

    /**
     * Get the property
     *
     * @param key          the key
     * @param defaultValue the default value
     * @param adapter      the adapter
     * @param <T>          the type of the value
     * @return the property
     */
    public <T> T getProperty(String key, T defaultValue, PropertyAdapter<T> adapter) {
        return adapter.get(this, key, defaultValue);
    }

    /**
     * Set the property
     *
     * @param key     the key
     * @param value   the value
     * @param adapter the adapter
     * @param <T>     the type of the value
     */
    public <T> void setProperty(String key, T value, PropertyAdapter<T> adapter) {
        adapter.set(this, key, value);
    }

    /**
     * Get the property
     *
     * @param keyValue the key-value pair
     * @param <T>      the type of the value
     * @return the property
     */
    public <T> T getProperty(PropertyKeyValue<T> keyValue) {
        return getProperty(keyValue.key(), keyValue.defaultValue(), keyValue.adapter());
    }

    /**
     * Set the property
     *
     * @param keyValue the key-value pair
     * @param value    the value
     * @param <T>      the type of the value
     */
    public <T> void setProperty(PropertyKeyValue<T> keyValue, T value) {
        setProperty(keyValue.key(), value, keyValue.adapter());
    }

    /**
     * Check if the property exists
     *
     * @param keyValue the key-value pair
     * @return true if the property exists
     */
    public boolean hasProperty(PropertyKeyValue<?> keyValue) {
        return hasProperty(keyValue.key());
    }

    /**
     * Get the serialized map
     *
     * @return the serialized map
     */
    public final Map<String, Object> getSerializedMap() {
        return serializedMap;
    }

    @SuppressWarnings("MethodDoesntCallSuperMethod")
    @Override
    public PropertyMap clone() {
        return create(serializedMap);
    }
}
