package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.configurate.ConfigurateConfig;
import me.hsgamer.hscore.config.proxy.ConfigGenerator;
import org.spongepowered.configurate.loader.HeaderMode;
import org.spongepowered.configurate.yaml.NodeStyle;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.io.File;

/**
 * The utility class for config
 */
@UtilityClass
public class ConfigUtil {
    /**
     * Check if the file is a config file
     *
     * @param file the file
     * @return true if it is a config file
     */
    public static boolean isConfigFile(File file) {
        return file.getName().endsWith(".yml") || file.getName().endsWith(".yaml");
    }

    /**
     * Get the config file
     *
     * @param parent the parent
     * @param name   the name
     * @return the config file
     */
    public static File getConfigFile(File parent, String name) {
        if (name.endsWith(".yml") || name.endsWith(".yaml")) {
            return new File(parent, name);
        }
        return new File(parent, name + ".yml");
    }

    /**
     * Get the config file
     *
     * @param name the name
     * @return the config file
     */
    public static File getConfigFile(String name) {
        return getConfigFile(new File("."), name);
    }

    /**
     * Generate a config
     *
     * @param file the file
     * @return the config
     */
    public static Config createConfig(File file) {
        if (!isConfigFile(file)) {
            throw new IllegalArgumentException("The file is not a config file");
        }
        return new ConfigurateConfig(file, YamlConfigurationLoader.builder()
                .nodeStyle(NodeStyle.BLOCK)
                .headerMode(HeaderMode.PRESERVE)
                .indent(2)
        );
    }

    /**
     * Generate a config object
     *
     * @param clazz       the class
     * @param file        the file
     * @param setupConfig whether to set up the config
     * @param stickyValue whether to cache the value
     * @param addDefault  whether to add default value
     * @param <T>         the type of the config object
     * @return the config object
     */
    public static <T> T generate(Class<T> clazz, File file, boolean setupConfig, boolean stickyValue, boolean addDefault) {
        return ConfigGenerator.newInstance(clazz, createConfig(file), setupConfig, stickyValue, addDefault);
    }

    /**
     * Generate a config object
     *
     * @param clazz       the class
     * @param file        the file
     * @param setupConfig whether to set up the config
     * @param stickyValue whether to cache the value
     * @param <T>         the type of the config object
     * @return the config object
     */
    public static <T> T generate(Class<T> clazz, File file, boolean setupConfig, boolean stickyValue) {
        return generate(clazz, file, setupConfig, stickyValue, true);
    }

    /**
     * Generate a config object
     *
     * @param clazz       the class
     * @param file        the file
     * @param setupConfig whether to set up the config
     * @param <T>         the type of the config object
     * @return the config object
     */
    public static <T> T generate(Class<T> clazz, File file, boolean setupConfig) {
        return generate(clazz, file, setupConfig, false);
    }

    /**
     * Generate a config object
     *
     * @param clazz the class
     * @param file  the file
     * @param <T>   the type of the config object
     * @return the config object
     */
    public static <T> T generate(Class<T> clazz, File file) {
        return generate(clazz, file, true);
    }
}
