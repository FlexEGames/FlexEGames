package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.hscore.config.configurate.ConfigurateConfig;
import me.hsgamer.hscore.config.proxy.ConfigGenerator;
import org.spongepowered.configurate.loader.HeaderMode;
import org.spongepowered.configurate.yaml.NodeStyle;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.io.File;

@UtilityClass
public class YamlConfigGenerator {
    public static <T> T generate(Class<T> clazz, File file, boolean setupConfig, boolean stickyValue, boolean addDefault) {
        return ConfigGenerator.newInstance(clazz, new ConfigurateConfig(file, YamlConfigurationLoader.builder()
                .nodeStyle(NodeStyle.BLOCK)
                .headerMode(HeaderMode.PRESERVE)
                .indent(2)
        ), setupConfig, stickyValue, addDefault);
    }


    public static <T> T generate(Class<T> clazz, File file, boolean setupConfig, boolean stickyValue) {
        return generate(clazz, file, setupConfig, stickyValue, true);
    }

    public static <T> T generate(Class<T> clazz, File file, boolean setupConfig) {
        return generate(clazz, file, setupConfig, false);
    }

    public static <T> T generate(Class<T> clazz, File file) {
        return generate(clazz, file, true);
    }
}
