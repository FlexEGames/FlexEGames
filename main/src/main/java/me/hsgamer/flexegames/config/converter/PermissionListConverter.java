package me.hsgamer.flexegames.config.converter;

import me.hsgamer.flexegames.util.PermissionUtil;
import me.hsgamer.hscore.config.annotation.converter.Converter;
import net.minestom.server.permission.Permission;

import java.util.*;

public class PermissionListConverter implements Converter {
    @Override
    public Object convert(Object rawValue) {
        if (rawValue == null) return null;
        List<Map<String, Object>> list;
        if (rawValue instanceof List<?> rawList) {
            list = new ArrayList<>();
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
        } else if (rawValue instanceof Map<?, ?> rawMap) {
            Map<String, Object> map = new LinkedHashMap<>();
            for (var entry : rawMap.entrySet()) {
                map.put(Objects.toString(entry.getKey()), Objects.toString(entry.getValue()));
            }
            list = List.of(map);
        } else if (rawValue instanceof String rawString) {
            list = List.of(Map.of(PermissionUtil.PERMISSION_KEY, rawString));
        } else {
            list = null;
        }
        return list == null ? null : PermissionUtil.toPermission(list);
    }

    @Override
    public Object convertToRaw(Object o) {
        try {
            // noinspection unchecked
            return PermissionUtil.toMap((List<Permission>) o);
        } catch (ClassCastException e) {
            return null;
        }
    }
}
