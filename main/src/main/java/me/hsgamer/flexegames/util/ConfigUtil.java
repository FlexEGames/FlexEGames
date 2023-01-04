package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.proxy.ConfigGenerator;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

@UtilityClass
public class ConfigUtil {
    private static final Map<Config, Map<Class<?>, Object>> configTypesMap;

    static {
        configTypesMap = new ConcurrentHashMap<>();
    }

    /**
     * Get the config object from the class
     *
     * @param config the config
     * @param clazz  the class
     * @param <T>    the type of the config object
     * @return the config object
     */
    public static <T> T getConfig(Config config, Class<T> clazz) {
        boolean forceAddDefault = Boolean.parseBoolean(Objects.toString(config.get("add-default"), "false"));
        try {
            return clazz.cast(
                    configTypesMap
                            .computeIfAbsent(config, c -> new ConcurrentHashMap<>())
                            .computeIfAbsent(clazz, aClass -> ConfigGenerator.newInstance(clazz, config, false, true, forceAddDefault))
            );
        } catch (Exception e) {
            throw new IllegalStateException("Cannot create a new instance of " + clazz.getName(), e);
        }
    }
}
