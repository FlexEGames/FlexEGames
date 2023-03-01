package me.hsgamer.flexegames.config.converter;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

public class ComponentConverter extends StringToObjectConverter<Component> {
    public static Component fromString(String rawValue) {
        return LegacyComponentSerializer.legacyAmpersand().deserialize(rawValue);
    }

    public static String toString(Component value) {
        return LegacyComponentSerializer.legacyAmpersand().serialize(value);
    }

    @Override
    public Component deserialize(String rawValue) {
        return fromString(rawValue);
    }

    @Override
    public String serialize(Component value) {
        return toString(value);
    }
}
