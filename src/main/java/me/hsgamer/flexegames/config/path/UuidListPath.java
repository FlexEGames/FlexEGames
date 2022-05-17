package me.hsgamer.flexegames.config.path;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class UuidListPath extends StringListToObjectPath<List<UUID>> {
    public UuidListPath(@NotNull String path, @Nullable List<UUID> def) {
        super(path, def);
    }

    @Override
    public @Nullable List<UUID> convert(@NotNull List<String> rawValue) {
        List<UUID> uuids = new ArrayList<>();
        for (String s : rawValue) {
            try {
                uuids.add(UUID.fromString(s));
            } catch (Exception ignored) {
                // IGNORED
            }
        }
        return uuids;
    }

    @Override
    public @Nullable List<String> convertToRaw(@NotNull List<UUID> value) {
        List<String> uuids = new ArrayList<>();
        for (UUID uuid : value) {
            uuids.add(uuid.toString());
        }
        return uuids;
    }
}
