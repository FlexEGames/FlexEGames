package me.hsgamer.flexegames.config.path;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class StringListPath extends StringListToObjectPath<List<String>> {
    public StringListPath(@NotNull String path, @Nullable List<String> def) {
        super(path, def);
    }

    @Nullable
    @Override
    public List<String> convert(@NotNull List<String> rawValue) {
        return rawValue;
    }

    @Override
    public @Nullable List<String> convertToRaw(@NotNull List<String> value) {
        return value;
    }
}
