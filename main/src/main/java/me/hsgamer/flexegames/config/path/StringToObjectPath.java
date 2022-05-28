package me.hsgamer.flexegames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;

public abstract class StringToObjectPath<T> extends AdvancedConfigPath<String, T> {
    protected StringToObjectPath(@NotNull String path, @Nullable T def) {
        super(path, def);
    }

    @Override
    public @Nullable String getFromConfig(@NotNull Config config) {
        return Optional.ofNullable(config.get(getPath())).map(Objects::toString).orElse(null);
    }
}
