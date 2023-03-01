package me.hsgamer.flexegames.api.property;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class GamePropertyMap implements Cloneable {
    private final Map<String, Object> serializedMap = new ConcurrentHashMap<>();

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

    public <T> T getProperty(String key, T defaultValue, GamePropertyAdapter<T> adapter) {
        return adapter.get(this, key, defaultValue);
    }

    public <T> void setProperty(String key, T value, GamePropertyAdapter<T> adapter) {
        adapter.set(this, key, value);
    }

    public <T> T getProperty(GamePropertyKeyValue<T> keyValue) {
        return getProperty(keyValue.key(), keyValue.defaultValue(), keyValue.adapter());
    }

    public <T> void setProperty(GamePropertyKeyValue<T> keyValue, T value) {
        setProperty(keyValue.key(), value, keyValue.adapter());
    }

    public final Map<String, Object> getSerializedMap() {
        return serializedMap;
    }

    @SuppressWarnings("MethodDoesntCallSuperMethod")
    @Override
    public GamePropertyMap clone() {
        return create(serializedMap);
    }
}
