package me.hsgamer.flexegames.config.path;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public class UuidPath extends StringToObjectPath<UUID> {
    public UuidPath(@NotNull String path, @Nullable UUID def) {
        super(path, def);
    }

    @Override
    public @Nullable UUID convert(@NotNull String rawValue) {
        try {
            return UUID.fromString(rawValue);
        } catch (Exception ignored) {
            return null;
        }
    }

    @Override
    public @Nullable String convertToRaw(@NotNull UUID value) {
        return value.toString();
    }
}
