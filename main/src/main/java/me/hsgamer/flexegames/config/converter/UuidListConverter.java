package me.hsgamer.flexegames.config.converter;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class UuidListConverter extends StringListToObjectConverter<List<UUID>> {
    @Override
    protected List<UUID> deserialize(List<String> rawValue) {
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
    protected List<String> serialize(List<UUID> value) {
        List<String> uuids = new ArrayList<>();
        for (UUID uuid : value) {
            uuids.add(uuid.toString());
        }
        return uuids;
    }
}
