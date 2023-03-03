package me.hsgamer.flexegames.api.property;

/**
 * A record class for a property key-value in {@link PropertyMap}
 *
 * @param key          the key
 * @param defaultValue the default value
 * @param adapter      the adapter
 * @param <T>          the type of the value
 */
public record PropertyKeyValue<T>(String key, T defaultValue, PropertyAdapter<T> adapter) {
    /**
     * Get the value from the map
     *
     * @param map the map
     * @return the value or the default value if the map doesn't have the key
     */
    public T get(PropertyMap map) {
        return adapter.get(map, key, defaultValue);
    }

    /**
     * Set the value to the map
     *
     * @param map   the map
     * @param value the value
     */
    public void set(PropertyMap map, T value) {
        adapter.set(map, key, value);
    }

    /**
     * Check if the map has the key
     *
     * @param map the map
     * @return true if the map has the key
     */
    public boolean has(PropertyMap map) {
        return map.hasProperty(key);
    }
}
