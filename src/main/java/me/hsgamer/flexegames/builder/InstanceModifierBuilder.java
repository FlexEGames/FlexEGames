package me.hsgamer.flexegames.builder;

import me.hsgamer.flexegames.api.modifier.InstanceModifierProvider;
import me.hsgamer.flexegames.modifier.ImageInstanceModifier;
import me.hsgamer.hscore.builder.Builder;

import java.util.Map;
import java.util.Optional;

public class InstanceModifierBuilder extends Builder<Map<String, Object>, InstanceModifierProvider> {
    public static final InstanceModifierBuilder INSTANCE = new InstanceModifierBuilder();

    private InstanceModifierBuilder() {
        register(ImageInstanceModifier::new, "image");
    }

    public static Optional<InstanceModifierProvider> buildInstanceModifier(Map<String, Object> map) {
        return Optional.ofNullable(map.get("type"))
                .map(String::valueOf)
                .flatMap(type -> INSTANCE.build(type, map));
    }
}
