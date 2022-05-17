package me.hsgamer.flexegames.config.path;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;

public class ComponentPath extends StringToObjectPath<Component> {
    public ComponentPath(@NotNull String path, @Nullable Component def) {
        super(path, def);
    }

    public ComponentPath(@NotNull String path, @Nullable String def) {
        super(path, Optional.ofNullable(def).map(LegacyComponentSerializer.legacyAmpersand()::deserialize).orElse(null));
    }

    @Override
    public @Nullable Component convert(@NotNull String rawValue) {
        return LegacyComponentSerializer.legacyAmpersand().deserialize(rawValue);
    }

    @Override
    public @Nullable String convertToRaw(@NotNull Component value) {
        return LegacyComponentSerializer.legacyAmpersand().serialize(value);
    }
}
