package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.api.game.Template;
import me.hsgamer.flexegames.builder.TemplateBuilder;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringHashMap;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.minestom.server.MinecraftServer;

import java.io.File;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public class TemplateManager {
    private final File templateFolder;
    private final Map<String, Template> templateMap = new CaseInsensitiveStringHashMap<>();
    private final Map<String, Config> configMap = new CaseInsensitiveStringHashMap<>();

    public TemplateManager() {
        templateFolder = new File("templates");
        if (!templateFolder.exists() && templateFolder.mkdirs()) {
            MinecraftServer.LOGGER.info("ArenaGame folder created");
        }
    }

    public void prepare() {
        for (File file : Objects.requireNonNull(templateFolder.listFiles())) {
            if (file.isDirectory() || !file.getName().endsWith(".yml")) {
                MinecraftServer.LOGGER.warn("{} is not a valid template file", file.getName());
                continue;
            }
            String name = file.getName().replace(".yml", "");
            Config config = new YamlProvider().loadConfiguration(file);
            configMap.put(name, config);
        }
    }

    public void init() {
        templateMap.clear();
        configMap.forEach((name, config) -> {
            Optional<Template> optional = TemplateBuilder.buildTemplate(config);
            if (optional.isPresent()) {
                templateMap.put(name, optional.get());
            } else {
                MinecraftServer.LOGGER.warn("ArenaGame {} is not a valid template", name);
            }
        });
    }

    public Optional<Template> getTemplate(String name) {
        return Optional.ofNullable(templateMap.get(name));
    }

    public Optional<Config> getConfig(String name) {
        return Optional.ofNullable(configMap.get(name));
    }

    public Map<String, Template> getTemplateMap() {
        return Collections.unmodifiableMap(templateMap);
    }

    public Map<String, Config> getConfigMap() {
        return Collections.unmodifiableMap(configMap);
    }
}
