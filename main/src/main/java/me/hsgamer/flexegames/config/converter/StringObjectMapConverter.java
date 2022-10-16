package me.hsgamer.flexegames.config.converter;

import java.util.Map;

public class StringObjectMapConverter extends StringObjectMapToObjectConverter<Map<String, Object>> {
    @Override
    protected Map<String, Object> deserialize(Map<String, Object> rawValue) {
        return rawValue;
    }

    @Override
    protected Map<String, Object> serialize(Map<String, Object> value) {
        return value;
    }
}
