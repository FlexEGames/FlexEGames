package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.builder.GameBuilder;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringHashMap;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;

import java.io.File;
import java.util.*;
import java.util.function.Predicate;

/**
 * The {@link Game} manager
 */
public class GameManager {
    private final GameServer gameServer;
    private final File gameFolder;
    private final Map<String, Game> gameMap = new CaseInsensitiveStringHashMap<>();
    private final Map<String, Config> configMap = new CaseInsensitiveStringHashMap<>();

    public GameManager(GameServer gameServer) {
        this.gameServer = gameServer;
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
            Optional<Game> optional = GameBuilder.buildGame(gameServer, config);
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
        gameMap.values().forEach(game -> {
            game.init();
            game.postInit();
        });
    }

    public void clear() {
        gameMap.values().forEach(Game::clear);
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

    /**
     * Get the list of all {@link Arena}s
     *
     * @return the list of all {@link Arena}s
     */
    public List<Arena> getAllArenas() {
        List<Arena> arenaList = new ArrayList<>();
        gameMap.values().forEach(game -> arenaList.addAll(game.getAllArenas()));
        return arenaList;
    }

    /**
     * Find the arenas that match the predicate
     *
     * @param predicate the predicate
     * @return the list of the arenas
     */
    public List<Arena> findArenas(Predicate<Arena> predicate) {
        return getAllArenas().stream().filter(predicate).toList();
    }

    /**
     * Find the arenas that match the owner predicate
     *
     * @param ownerPredicate the predicate to check the owner
     * @return the list of the arenas
     */
    public List<Arena> findArenasByOwner(Predicate<UUID> ownerPredicate) {
        return gameMap.values().stream().map(game -> game.findArenasByOwner(ownerPredicate)).flatMap(Collection::stream).toList();
    }

    /**
     * Find the arenas of the owner
     *
     * @param player the owner
     * @return the list of the arenas
     */
    public List<Arena> findArenasByOwner(Player player) {
        return findArenasByOwner(uuid -> player.getUuid().equals(uuid));
    }

    /**
     * Find the arenas by the list of the owners
     *
     * @param owners the list of the owners
     * @return the list of the arenas
     */
    public List<Arena> findArenasByOwner(List<UUID> owners) {
        return findArenasByOwner(owners::contains);
    }

    /**
     * Get the arena that the player is in
     *
     * @param player the player
     * @return the arena
     */
    public Optional<Arena> getJoinedArena(Player player) {
        return gameMap.values().stream().map(game -> game.getJoinedArena(player)).filter(Optional::isPresent).map(Optional::get).findFirst();
    }
}
