package me.hsgamer.epicmegagames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;

public class ComponentPath extends AdvancedConfigPath<String, Component> {
    public ComponentPath(@NotNull String path, @Nullable Component def) {
        super(path, def);
    }

    public ComponentPath(@NotNull String path, @Nullable String def) {
        super(path, Optional.ofNullable(def).map(LegacyComponentSerializer.legacyAmpersand()::deserialize).orElse(null));
    }

    @Override
    public @Nullable String getFromConfig(@NotNull Config config) {
        return Optional.ofNullable(config.get(getPath())).map(Objects::toString).orElse(null);
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
