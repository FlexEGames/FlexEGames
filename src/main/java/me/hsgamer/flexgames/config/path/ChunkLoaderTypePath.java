package me.hsgamer.flexgames.config.path;

import me.hsgamer.flexgames.api.ChunkLoaderType;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class ChunkLoaderTypePath extends AdvancedConfigPath<String, ChunkLoaderType> {
    public ChunkLoaderTypePath(@NotNull String path, @Nullable ChunkLoaderType def) {
        super(path, def);
    }

    @Override
    public @Nullable String getFromConfig(@NotNull Config config) {
        return Objects.toString(config.get(getPath()), null);
    }

    @Override
    public @Nullable ChunkLoaderType convert(@NotNull String rawValue) {
        try {
            return ChunkLoaderType.valueOf(rawValue.toUpperCase());
        } catch (IllegalArgumentException ignored) {
            return null;
        }
    }

    @Override
    public @Nullable String convertToRaw(@NotNull ChunkLoaderType value) {
        return value.name();
    }
}
