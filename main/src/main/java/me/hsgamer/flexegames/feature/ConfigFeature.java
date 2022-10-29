package me.hsgamer.flexegames.feature;

import lombok.Getter;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.proxy.ConfigGenerator;
import me.hsgamer.minigamecore.base.Feature;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The game config feature
 */
public class ConfigFeature implements Feature {
    @Getter
    private final Config config;
    private final Map<Class<?>, Object> configTypesMap;

    public ConfigFeature(Config config) {
        this.config = config;
        this.configTypesMap = new ConcurrentHashMap<>();
    }

    /**
     * Get the config object from the class
     *
     * @param clazz      the class
     * @param addDefault whether to add the default value to the config
     * @param <T>        the type of the config object
     * @return the config object
     */
    public <T> T getConfig(Class<T> clazz, boolean addDefault) {
        boolean forceAddDefault = Boolean.parseBoolean(Objects.toString(config.get("add-default"), "false"));
        try {
            return clazz.cast(configTypesMap.computeIfAbsent(clazz, aClass -> ConfigGenerator.newInstance(clazz, config, false, true, addDefault || forceAddDefault)));
        } catch (Exception e) {
            throw new IllegalStateException("Cannot create a new instance of " + clazz.getName(), e);
        }
    }

    /**
     * Get the config object from the class
     *
     * @param clazz the class
     * @param <T>   the type of the config object
     * @return the config object
     */
    public <T> T getConfig(Class<T> clazz) {
        return getConfig(clazz, false);
    }
}
