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

    public Optional<Game> getGame(String name) {
        return Optional.ofNullable(gameMap.get(name));
    }

    public Optional<Config> getConfig(String name) {
        return Optional.ofNullable(configMap.get(name));
    }

    public Map<String, Game> getGameMap() {
        return Collections.unmodifiableMap(gameMap);
    }

    public Map<String, Config> getConfigMap() {
        return Collections.unmodifiableMap(configMap);
    }

    public List<Arena> getAllArenas() {
        List<Arena> arenaList = new ArrayList<>();
        gameMap.values().forEach(game -> arenaList.addAll(game.getAllArenas()));
        return arenaList;
    }

    public List<Arena> findArenas(Predicate<Arena> predicate) {
        return getAllArenas().stream().filter(predicate).toList();
    }

    public List<Arena> findArenasByOwner(Predicate<UUID> ownerPredicate) {
        return gameMap.values().stream().map(game -> game.findArenasByOwner(ownerPredicate)).flatMap(Collection::stream).toList();
    }

    public List<Arena> findArenasByOwner(Player player) {
        return findArenasByOwner(uuid -> player.getUuid().equals(uuid));
    }

    public List<Arena> findArenasByOwner(List<UUID> owners) {
        return findArenasByOwner(owners::contains);
    }


    public Optional<Arena> getJoinedArena(Player player) {
        return gameMap.values().stream().map(game -> game.getJoinedArena(player)).filter(Optional::isPresent).map(Optional::get).findFirst();
    }
}
