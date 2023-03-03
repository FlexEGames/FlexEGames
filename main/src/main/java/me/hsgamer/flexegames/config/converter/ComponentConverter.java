package me.hsgamer.flexegames.config.converter;

import me.hsgamer.flexegames.util.ComponentUtil;
import net.kyori.adventure.text.Component;

public class ComponentConverter extends StringToObjectConverter<Component> {
    @Override
    public Component deserialize(String rawValue) {
        return ComponentUtil.fromString(rawValue);
    }

    @Override
    public String serialize(Component value) {
        return ComponentUtil.toString(value);
    }
}
