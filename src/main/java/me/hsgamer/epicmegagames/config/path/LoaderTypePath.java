package me.hsgamer.epicmegagames.config.path;

import me.hsgamer.epicmegagames.util.LoaderType;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class LoaderTypePath extends AdvancedConfigPath<String, LoaderType> {
    public LoaderTypePath(@NotNull String path, @Nullable LoaderType def) {
        super(path, def);
    }

    @Override
    public @Nullable String getFromConfig(@NotNull Config config) {
        return Objects.toString(config.get(getPath()), null);
    }

    @Override
    public @Nullable LoaderType convert(@NotNull String rawValue) {
        try {
            return LoaderType.valueOf(rawValue.toUpperCase());
        } catch (IllegalArgumentException ignored) {
            return null;
        }
    }

    @Override
    public @Nullable String convertToRaw(@NotNull LoaderType value) {
        return value.name();
    }
}
