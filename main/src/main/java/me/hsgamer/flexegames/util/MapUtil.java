package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;

import java.util.LinkedHashMap;
import java.util.Map;

@UtilityClass
public class MapUtil {
    public static Map<String, Object> toStringObjectMap(Map<?, ?> map) {
        Map<String, Object> newMap = new LinkedHashMap<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            newMap.put(entry.getKey().toString(), entry.getValue());
        }
        return newMap;
    }
}
