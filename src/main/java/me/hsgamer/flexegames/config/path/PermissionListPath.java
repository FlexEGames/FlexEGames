package me.hsgamer.flexegames.config.path;

import me.hsgamer.flexegames.util.PermissionUtil;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.minestom.server.permission.Permission;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class PermissionListPath extends AdvancedConfigPath<List<Map<String, Object>>, List<Permission>> {
    public PermissionListPath(@NotNull String path, @Nullable List<Permission> def) {
        super(path, def);
    }

    @Override
    public @Nullable List<Map<String, Object>> getFromConfig(@NotNull Config config) {
        if (!config.contains(getPath())) return null;
        var rawValue = config.get(getPath());
        if (rawValue instanceof List<?> rawList) {
            List<Map<String, Object>> list = new ArrayList<>();
            for (var o : rawList) {
                if (o instanceof Map<?, ?> rawMap) {
                    Map<String, Object> map = new LinkedHashMap<>();
                    for (Map.Entry<?, ?> entry : rawMap.entrySet()) {
                        map.put(Objects.toString(entry.getKey()), entry.getValue());
                    }
                    list.add(map);
                } else if (o instanceof String rawString) {
                    list.add(Map.of(PermissionUtil.PERMISSION_KEY, rawString));
                }
            }
            return list;
        } else if (rawValue instanceof Map<?, ?> rawMap) {
            Map<String, Object> map = new LinkedHashMap<>();
            for (var entry : rawMap.entrySet()) {
                map.put(Objects.toString(entry.getKey()), Objects.toString(entry.getValue()));
            }
            return List.of(map);
        } else if (rawValue instanceof String rawString) {
            return List.of(Map.of(PermissionUtil.PERMISSION_KEY, rawString));
        }
        return null;
    }

    @Override
    public @Nullable List<Permission> convert(@NotNull List<Map<String, Object>> rawValue) {
        return PermissionUtil.toPermission(rawValue);
    }

    @Override
    public @Nullable List<Map<String, Object>> convertToRaw(@NotNull List<Permission> value) {
        return PermissionUtil.toMap(value);
    }
}
