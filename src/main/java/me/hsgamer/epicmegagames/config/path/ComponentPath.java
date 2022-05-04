package me.hsgamer.epicmegagames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class ComponentPath extends AdvancedConfigPath<String, Component> {
    public ComponentPath(@NotNull String path, @Nullable Component def) {
        super(path, def);
    }

    @Override
    public @Nullable String getFromConfig(@NotNull Config config) {
        return Objects.toString(config.get(getPath()), "");
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
