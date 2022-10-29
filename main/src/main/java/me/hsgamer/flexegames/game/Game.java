package me.hsgamer.flexegames.game;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.feature.*;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import net.minestom.server.entity.Player;

import java.util.*;
import java.util.function.Predicate;

/**
 * The main class of the game.
 * It contains all the features and the arenas.
 */
public abstract class Game extends ArenaManager {
    protected final GameServer gameServer;
    protected final Config gameConfig;

    /**
     * Create a new game
     *
     * @param gameServer the game server
     * @param gameConfig the game config
     */
    protected Game(GameServer gameServer, Config gameConfig) {
        this.gameServer = gameServer;
        this.gameConfig = gameConfig;
    }

    /**
     * Get the game server
     *
     * @param pair the pair of the game server and the game config
     */
    protected Game(Pair<GameServer, Config> pair) {
        this(pair.getKey(), pair.getValue());
    }

    @Override
    protected final List<Feature> loadFeatures() {
        List<Feature> features = new ArrayList<>();
        features.add(new ConfigFeature(gameConfig));
        features.add(new LobbyFeature(gameServer));
        features.add(new GameServerFeature(gameServer));
        features.add(new OwnerFeature());
        features.add(new DescriptionFeature(this));
        features.add(new JoinFeature());
        features.addAll(getFeatures());
        return features;
    }

    /**
     * Create a new arena
     *
     * @param name  the name of the arena
     * @param owner the owner of the arena
     * @return the arena
     */
    public Arena createArena(String name, UUID owner) {
        GameArena arena = new GameArena(name, this);
        arena.setNextState(getInitialState());
        arena.getArenaFeature(OwnerFeature.class).setOwner(owner);
        configureArena(arena);
        addArena(arena);
        return arena;
    }

    /**
     * Create a new arena
     *
     * @param owner the owner of the arena
     * @return the arena
     */
    public Arena createArena(UUID owner) {
        return createArena(UUID.randomUUID().toString(), owner);
    }

    /**
     * Check if the game is configured properly.
     * Override this to add more checks.
     *
     * @return true if the game is configured properly
     */
    public boolean isConfigured() {
        return true;
    }

    /**
     * Get the additional features of the game.
     * Override this to add more features.
     *
     * @return the additional features
     */
    protected List<Feature> getFeatures() {
        return Collections.emptyList();
    }

    /**
     * Get the initial {@link GameState} of the arena.
     * This is the first {@link GameState} will be set when the arena is created.
     *
     * @return the initial {@link GameState}
     */
    protected abstract Class<? extends GameState> getInitialState();

    /**
     * Configure the arena.
     * Override this to add additional configurations for the arenas.
     *
     * @param arena the arena
     */
    protected void configureArena(Arena arena) {
        // EMPTY
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
        return getAllArenas().stream().filter(arena -> ownerPredicate.test(arena.getArenaFeature(OwnerFeature.class).getOwner())).toList();
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
        return getAllArenas().stream().filter(arena -> arena.getArenaFeature(JoinFeature.class).isJoined(player)).findFirst();
    }
}
