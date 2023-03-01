package me.hsgamer.flexegames.api.property;

public record PropertyKeyValue<T>(String key, T defaultValue, PropertyAdapter<T> adapter) {
    public T get(PropertyMap map) {
        return adapter.get(map, key, defaultValue);
    }

    public void set(PropertyMap map, T value) {
        adapter.set(map, key, value);
    }

    public boolean has(PropertyMap map) {
        return map.hasProperty(key);
    }
}
