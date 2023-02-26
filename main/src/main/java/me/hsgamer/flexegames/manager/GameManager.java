package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.builder.GameBuilder;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringHashMap;
import me.hsgamer.hscore.config.Config;
import net.minestom.server.MinecraftServer;

import java.io.File;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * The {@link Game} manager
 */
// TODO: remove configMap, add Property Map per Player & apply the new Game Property API
public class GameManager {
    private final File gameFolder;
    private final Map<String, Game> gameMap = new CaseInsensitiveStringHashMap<>();
    private final Map<String, Config> configMap = new CaseInsensitiveStringHashMap<>();

    public GameManager() {
        gameFolder = new File("games");
        if (!gameFolder.exists() && gameFolder.mkdirs()) {
            MinecraftServer.LOGGER.info("Game folder created");
        }
    }

    public void prepare() {
        for (File file : Objects.requireNonNull(gameFolder.listFiles())) {
            if (file.isDirectory() || !file.getName().endsWith(".yml")) {
                MinecraftServer.LOGGER.warn("{} is not a valid game file", file.getName());
                continue;
            }
            String name = file.getName().replace(".yml", "");
            Config config = YamlConfigGenerator.createConfig(file);
            config.setup();
            configMap.put(name, config);
        }
    }

    public void init() {
        configMap.forEach((name, config) -> {
            Optional<Game> optional = GameBuilder.buildGame(config);
            if (optional.isEmpty()) {
                MinecraftServer.LOGGER.warn("ArenaGame {} is not a valid template", name);
                return;
            }
            Game game = optional.get();
            if (game.isConfigured()) {
                gameMap.put(name, game);
            } else {
                MinecraftServer.LOGGER.warn("{} is not configured", name);
            }
        });
    }

    public void clear() {
        gameMap.clear();
        configMap.clear();
    }

    /**
     * Get the {@link Game} by name
     *
     * @param name the name
     * @return the {@link Game}
     */
    public Optional<Game> getGame(String name) {
        return Optional.ofNullable(gameMap.get(name));
    }

    /**
     * Get the {@link Config} by name
     *
     * @param name the name
     * @return the {@link Config}
     */
    public Optional<Config> getConfig(String name) {
        return Optional.ofNullable(configMap.get(name));
    }

    /**
     * Get the {@link Game} map
     *
     * @return the {@link Game} map
     */
    public Map<String, Game> getGameMap() {
        return Collections.unmodifiableMap(gameMap);
    }

    /**
     * Get the {@link Config} map
     *
     * @return the {@link Config} map
     */
    public Map<String, Config> getConfigMap() {
        return Collections.unmodifiableMap(configMap);
    }
}
