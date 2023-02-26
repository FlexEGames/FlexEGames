package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.flexegames.feature.GameServerFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.feature.arena.OwnerFeature;
import me.hsgamer.flexegames.state.KillingState;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import net.minestom.server.entity.Player;

import java.util.*;
import java.util.function.Predicate;

/**
 * The {@link ArenaManager} for {@link Game}
 */
public class GameArenaManager extends ArenaManager {
    private final GameServer gameServer;

    public GameArenaManager(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    public static boolean isValid(Arena arena) {
        return arena.getFeature(DescriptionFeature.class) != null
                && arena.getFeature(GameFeature.class) != null
                && arena.getFeature(JoinFeature.class) != null
                && arena.getFeature(OwnerFeature.class) != null;
    }

    @Override
    protected List<GameState> loadGameStates() {
        return Collections.singletonList(
                new KillingState(gameServer)
        );
    }

    @Override
    protected List<Feature> loadFeatures() {
        return Arrays.asList(
                new GameServerFeature(gameServer),
                new LobbyFeature(gameServer)
        );
    }

    @Override
    public boolean addArena(Arena arena) {
        if (!super.addArena(arena)) {
            return false;
        }
        if (!isValid(arena)) {
            arena.removeFromManager();
            return false;
        }
        return true;
    }

    /**
     * Create the {@link Arena} of the {@link Game}
     *
     * @param name  the name of the {@link Arena}
     * @param game  the {@link Game}
     * @param owner the owner of the {@link Arena}
     * @return the {@link Arena}
     */
    public Arena createArena(String name, Game game, GamePropertyMap propertyMap, UUID owner) {
        var arena = game.create(name, propertyMap, this, owner);
        if (addArena(arena)) {
            arena.postInit();
            return arena;
        } else {
            throw new IllegalStateException("Cannot create arena");
        }
    }

    /**
     * Create the {@link Arena} of the {@link Game}
     *
     * @param game  the {@link Game}
     * @param owner the owner of the {@link Arena}
     * @return the {@link Arena}
     */
    public Arena createArena(Game game, GamePropertyMap propertyMap, UUID owner) {
        String name;
        do {
            name = UUID.randomUUID().toString();
        } while (getArenaByName(name).isPresent());
        return createArena(name, game, propertyMap, owner);
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
        return getAllArenas().stream().filter(arena -> ownerPredicate.test(arena.getFeature(OwnerFeature.class).owner())).toList();
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
        return getAllArenas().stream().filter(arena -> arena.getFeature(JoinFeature.class).isJoined(player)).findFirst();
    }
}
