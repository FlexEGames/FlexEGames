package me.hsgamer.flexegames.api.property;

public final class GamePropertyKeyValue<T> {
    private final String key;
    private final GamePropertyAdapter<T> adapter;
    private T defaultValue;

    public GamePropertyKeyValue(String key, T defaultValue, GamePropertyAdapter<T> adapter) {
        this.key = key;
        this.defaultValue = defaultValue;
        this.adapter = adapter;
    }

    public T get(GamePropertyMap map) {
        return adapter.get(map, key, defaultValue);
    }

    public void set(GamePropertyMap map, T value) {
        adapter.set(map, key, value);
    }

    public String key() {
        return key;
    }

    public T defaultValue() {
        return defaultValue;
    }

    public void defaultValue(T defaultValue) {
        this.defaultValue = defaultValue;
    }

    public GamePropertyAdapter<T> adapter() {
        return adapter;
    }
}
