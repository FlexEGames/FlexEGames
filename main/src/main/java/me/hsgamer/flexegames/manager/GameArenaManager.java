package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.feature.GameServerFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.feature.arena.OwnerFeature;
import me.hsgamer.flexegames.state.KillingState;
import me.hsgamer.minigamecore.base.*;
import net.minestom.server.entity.Player;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Predicate;

/**
 * The {@link ArenaManager} for {@link Game}
 */
public class GameArenaManager extends ArenaManager {
    private final GameServer gameServer;

    public GameArenaManager(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    @Override
    protected List<Unit<GameState>> loadGameStates() {
        return Unit.wrap(
                new KillingState(gameServer)
        );
    }

    @Override
    protected List<Unit<Feature>> loadFeatures() {
        return Unit.wrap(
                new GameServerFeature(gameServer),
                new LobbyFeature(gameServer)
        );
    }

    @Override
    public boolean addArena(Arena arena) {
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        var gameFeature = arena.getFeature(GameFeature.class);
        var joinFeature = arena.getFeature(JoinFeature.class);
        var ownerFeature = arena.getFeature(OwnerFeature.class);
        if (descriptionFeature == null || gameFeature == null || joinFeature == null || ownerFeature == null) {
            return false;
        }
        return super.addArena(arena);
    }

    /**
     * Create the {@link Arena} of the {@link Game}
     *
     * @param name  the name of the {@link Arena}
     * @param game  the {@link Game}
     * @param owner the owner of the {@link Arena}
     * @return the {@link Arena}
     */
    public Arena createArena(String name, Game game, UUID owner) {
        var arena = game.createArena(name).apply(this);
        if (addArena(arena)) {
            arena.getFeature(OwnerFeature.class).setOwner(owner);
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
    public Arena createArena(Game game, UUID owner) {
        String name;
        do {
            name = UUID.randomUUID().toString();
        } while (getArenaByName(name).isPresent());
        return createArena(name, game, owner);
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
        return getAllArenas().stream().filter(arena -> ownerPredicate.test(arena.getFeature(OwnerFeature.class).getOwner())).toList();
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
