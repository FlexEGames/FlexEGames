package me.hsgamer.flexegames.config.converter;

import java.util.UUID;

public class UuidConverter extends StringToObjectConverter<UUID> {
    @Override
    public UUID deserialize(String rawValue) {
        try {
            return UUID.fromString(rawValue);
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public String serialize(UUID value) {
        return value.toString();
    }
}
