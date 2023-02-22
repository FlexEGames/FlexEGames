package me.hsgamer.flexegames.api.property;

import java.util.HashMap;
import java.util.Map;

public class GamePropertyMap {
    private final Map<String, Object> serializedMap = new HashMap<>();

    private GamePropertyMap() {
        // EMPTY
    }

    public static GamePropertyMap create() {
        return new GamePropertyMap();
    }

    public static GamePropertyMap create(Map<String, Object> propertyMap) {
        GamePropertyMap gameProperties = new GamePropertyMap();
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

    public <T> T getProperty(String key, GamePropertyAdapter<T> adapter) {
        return adapter.get(this, key);
    }

    public <T> void setProperty(String key, T value, GamePropertyAdapter<T> adapter) {
        adapter.set(this, key, value);
    }

    public final Map<String, Object> getSerializedMap() {
        return serializedMap;
    }
}
