package me.hsgamer.flexegames.game;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.feature.*;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import net.minestom.server.entity.Player;

import java.util.*;
import java.util.function.Predicate;

public abstract class Game extends ArenaManager {
    protected final GameServer gameServer;
    protected final Config gameConfig;

    protected Game(GameServer gameServer, Config gameConfig) {
        this.gameServer = gameServer;
        this.gameConfig = gameConfig;
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

    public Arena createArena(String name, UUID owner) {
        GameArena arena = new GameArena(name, this);
        arena.setNextState(getInitialState());
        arena.getArenaFeature(OwnerFeature.class).setOwner(owner);
        configureArena(arena);
        addArena(arena);
        return arena;
    }

    public Arena createArena(UUID owner) {
        String name;
        do {
            name = UUID.randomUUID().toString();
        } while (getArenaByName(name).isPresent());
        return createArena(name, owner);
    }

    public boolean isConfigured() {
        return true;
    }

    protected List<Feature> getFeatures() {
        return Collections.emptyList();
    }

    protected abstract Class<? extends GameState> getInitialState();

    protected abstract void configureArena(Arena arena);

    public abstract Optional<Arena> getJoinedArena(Player player);

    public List<Arena> findArenas(Predicate<Arena> predicate) {
        return getAllArenas().stream().filter(predicate).toList();
    }

    public List<Arena> findArenasByOwner(Predicate<UUID> ownerPredicate) {
        return getAllArenas().stream().filter(arena -> ownerPredicate.test(arena.getArenaFeature(OwnerFeature.class).getOwner())).toList();
    }

    public List<Arena> findArenasByOwner(Player player) {
        return findArenasByOwner(uuid -> player.getUuid().equals(uuid));
    }

    public List<Arena> findArenasByOwner(List<UUID> owners) {
        return findArenasByOwner(owners::contains);
    }
}
