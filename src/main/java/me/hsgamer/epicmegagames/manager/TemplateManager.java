package me.hsgamer.epicmegagames.manager;

import me.hsgamer.epicmegagames.api.TemplateProvider;
import me.hsgamer.epicmegagames.builder.TemplateProviderBuilder;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringHashMap;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.minestom.server.MinecraftServer;

import java.io.File;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public class TemplateManager {
    private final File templateFolder;
    private final Map<String, TemplateProvider> templateMap = new CaseInsensitiveStringHashMap<>();

    public TemplateManager() {
        templateFolder = new File("templates");
        if (!templateFolder.exists() && templateFolder.mkdirs()) {
            MinecraftServer.LOGGER.info("Template folder created");
        }
    }

    public void init() {
        templateMap.clear();
        for (File file : Objects.requireNonNull(templateFolder.listFiles())) {
            if (file.isDirectory() || !file.getName().endsWith(".yml")) {
                MinecraftServer.LOGGER.warn("{} is not a valid template file", file.getName());
                continue;
            }
            String name = file.getName().replace(".yml", "");
            Config config = new YamlProvider().loadConfiguration(file);
            Optional<TemplateProvider> optional = Optional.ofNullable(config.get("type"))
                    .map(String::valueOf)
                    .flatMap(type -> TemplateProviderBuilder.INSTANCE.build(type, config));
            if (optional.isPresent()) {
                templateMap.put(name, optional.get());
            } else {
                MinecraftServer.LOGGER.warn("Template {} is not a valid template", name);
            }
        }
    }
}
