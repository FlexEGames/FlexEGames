package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

import java.util.List;

@UtilityClass
public class ComponentUtil {
    public static Component stripItalics(Component component) {
        if (component == null) return null;

        if (component.decoration(TextDecoration.ITALIC) == TextDecoration.State.NOT_SET) {
            component = component.decoration(TextDecoration.ITALIC, false);
        }

        return component;
    }

    public static Component fromString(String rawValue) {
        return LegacyComponentSerializer.legacyAmpersand().deserialize(rawValue);
    }

    public static String toString(Component value) {
        return LegacyComponentSerializer.legacyAmpersand().serialize(value);
    }

    public static List<Component> fromStringList(List<String> rawValue) {
        return rawValue.stream()
                .map(s -> LegacyComponentSerializer.legacyAmpersand().deserialize(s))
                .map(Component::asComponent)
                .toList();
    }
}
