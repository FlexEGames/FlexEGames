package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringHashMap;
import net.minestom.server.entity.Player;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

/**
 * The {@link Game} manager
 */
// TODO: add Property Map per Player & apply the new Game Property API
public class GameManager {
    private final GameServer gameServer;
    private final Map<String, Game> gameMap = new CaseInsensitiveStringHashMap<>();

    public GameManager(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    public CompletableFuture<Boolean> createArena(Player player, Game game) {
        return game.createProperty(player).exceptionally(throwable -> {
            player.sendMessage("Failed to create the arena"); // TODO
            return null;
        }).thenApply(property -> {
            if (property == null) {
                return false;
            }
            gameServer.getArenaManager().createArena(game, property, player.getUuid());
            return true;
        });
    }

    public void registerGame(String identifier, Game game) {
        if (gameMap.containsKey(identifier)) {
            throw new IllegalArgumentException("Game " + identifier + " already exists");
        }
        gameMap.put(identifier, game);
    }

    /**
     * Get the {@link Game} by identifier
     *
     * @param identifier the identifier
     * @return the {@link Game}
     */
    public Optional<Game> getGame(String identifier) {
        return Optional.ofNullable(gameMap.get(identifier));
    }

    /**
     * Get the {@link Game} map
     *
     * @return the {@link Game} map
     */
    public Map<String, Game> getGameMap() {
        return Collections.unmodifiableMap(gameMap);
    }
}
