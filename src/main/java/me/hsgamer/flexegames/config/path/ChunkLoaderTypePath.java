package me.hsgamer.flexegames.config.path;

import me.hsgamer.flexegames.api.chunk.ChunkLoaderType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ChunkLoaderTypePath extends StringToObjectPath<ChunkLoaderType> {
    public ChunkLoaderTypePath(@NotNull String path, @Nullable ChunkLoaderType def) {
        super(path, def);
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
