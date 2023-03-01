package me.hsgamer.flexegames.api.property;

public record GamePropertyKeyValue<T>(String key, T defaultValue, GamePropertyAdapter<T> adapter) {
    public T get(GamePropertyMap map) {
        return adapter.get(map, key, defaultValue);
    }

    public void set(GamePropertyMap map, T value) {
        adapter.set(map, key, value);
    }

    public boolean has(GamePropertyMap map) {
        return map.hasProperty(key);
    }
}
