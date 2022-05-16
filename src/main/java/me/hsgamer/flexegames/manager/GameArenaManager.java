package me.hsgamer.flexegames.manager;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.Template;
import me.hsgamer.flexegames.arena.GameArena;
import me.hsgamer.flexegames.config.MainConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.state.*;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.minestom.server.entity.Player;

import java.util.List;
import java.util.UUID;
import java.util.function.Predicate;

public class GameArenaManager extends ArenaManager {
    private final GameServer gameServer;

    public GameArenaManager(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    @Override
    protected List<GameState> loadGameStates() {
        return List.of(
                new ChoosingState(),
                new WaitingState(),
                new InGameState(),
                new EndingState(),
                new KillingState()
        );
    }

    @Override
    protected List<Feature> loadFeatures() {
        return List.of(
                new ArenaTimerFeature(),
                new GameFeature(),
                new LobbyFeature(gameServer)
        );
    }

    public Arena createNewArena() {
        String name;
        do {
            name = "Arena-" + UUID.randomUUID();
        } while (getArenaByName(name).isPresent());
        Arena arena = new GameArena(name, this);
        addArena(arena);
        return arena;
    }

    public List<Arena> findArenas(Predicate<Arena> predicate) {
        return getAllArenas().stream().filter(predicate).toList();
    }

    public List<Arena> findArenasByOwner(Predicate<UUID> ownerPredicate) {
        return getAllArenas().stream().filter(arena -> {
            GameFeature.ArenaGameFeature feature = arena.getArenaFeature(GameFeature.class);
            if (feature.getGame() == null) {
                return false;
            }
            return ownerPredicate.test(feature.getOwner());
        }).toList();
    }

    public List<Arena> findArenasByOwner(Player player) {
        return findArenasByOwner(uuid -> player.getUuid().equals(uuid));
    }

    public List<Arena> findArenasByOwner(List<UUID> owners) {
        return findArenasByOwner(owners::contains);
    }

    public List<Arena> findArenasByTemplate(Template template) {
        return getAllArenas().stream().filter(arena -> {
            GameFeature.ArenaGameFeature feature = arena.getArenaFeature(GameFeature.class);
            return feature.getGame() != null && feature.getGame().getTemplate() == template;
        }).toList();
    }

    public boolean createArena(Player player, Template template) {
        int amount = MainConfig.ARENA_AMOUNT_PER_PLAYER.getValue();
        if (amount >= 0 && findArenasByOwner(player).size() >= amount) {
            return false;
        }
        Arena arena = gameServer.getGameArenaManager().createNewArena();
        arena.getArenaFeature(GameFeature.class).setGame(template);
        arena.getArenaFeature(GameFeature.class).setOwner(player.getUuid());
        return true;
    }
}
