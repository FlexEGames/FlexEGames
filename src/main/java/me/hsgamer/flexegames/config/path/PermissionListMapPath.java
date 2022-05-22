package me.hsgamer.flexegames.config.path;

import me.hsgamer.flexegames.util.PermissionUtil;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.minestom.server.permission.Permission;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class PermissionListMapPath extends AdvancedConfigPath<Map<String, List<Map<String, Object>>>, Map<String, List<Permission>>> {
    public PermissionListMapPath(@NotNull String path, @Nullable Map<String, List<Permission>> def) {
        super(path, def);
    }

    @Override
    public @Nullable Map<String, List<Map<String, Object>>> getFromConfig(@NotNull Config config) {
        if (!config.contains(getPath())) return null;
        var rawMap = config.getNormalizedValues(getPath(), false);
        Map<String, List<Map<String, Object>>> map = new LinkedHashMap<>();
        for (var entry : rawMap.entrySet()) {
            if (entry.getValue() instanceof List<?> valueList) {
                List<Map<String, Object>> list = new ArrayList<>();
                for (var o : valueList) {
                    if (o instanceof Map<?, ?> rawPermMap) {
                        Map<String, Object> map1 = new LinkedHashMap<>();
                        for (Map.Entry<?, ?> entry1 : rawPermMap.entrySet()) {
                            map1.put(Objects.toString(entry1.getKey()), entry1.getValue());
                        }
                        list.add(map1);
                    } else if (o instanceof String) {
                        list.add(Map.of(PermissionUtil.PERMISSION_KEY, o));
                    }
                }
                map.put(entry.getKey(), list);
            } else if (entry.getValue() instanceof Map<?, ?>) {
                Map<String, Object> map1 = new LinkedHashMap<>();
                for (Map.Entry<?, ?> entry1 : ((Map<?, ?>) entry.getValue()).entrySet()) {
                    map1.put(Objects.toString(entry1.getKey()), entry1.getValue());
                }
                map.put(entry.getKey(), List.of(map1));
            } else if (entry.getValue() instanceof String) {
                map.put(entry.getKey(), List.of(Map.of(PermissionUtil.PERMISSION_KEY, entry.getValue())));
            }
        }
        return map;
    }

    @Override
    public @Nullable Map<String, List<Permission>> convert(@NotNull Map<String, List<Map<String, Object>>> rawValue) {
        Map<String, List<Permission>> map = new LinkedHashMap<>();
        for (var entry : rawValue.entrySet()) {
            map.put(entry.getKey(), PermissionUtil.toPermission(entry.getValue()));
        }
        return map;
    }

    @Override
    public @Nullable Map<String, List<Map<String, Object>>> convertToRaw(@NotNull Map<String, List<Permission>> value) {
        Map<String, List<Map<String, Object>>> map = new LinkedHashMap<>();
        for (var entry : value.entrySet()) {
            map.put(entry.getKey(), PermissionUtil.toMap(entry.getValue()));
        }
        return map;
    }
}
