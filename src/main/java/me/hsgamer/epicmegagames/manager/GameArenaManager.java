package me.hsgamer.epicmegagames.manager;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.epicmegagames.arena.GameArena;
import me.hsgamer.epicmegagames.feature.GameFeature;
import me.hsgamer.epicmegagames.feature.LobbyFeature;
import me.hsgamer.epicmegagames.state.*;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.minestom.server.entity.Player;

import java.util.List;
import java.util.UUID;

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

    public List<Arena> getArenas(Player player, boolean myArena) {
        return getAllArenas().stream().filter(arena -> {
            GameFeature.ArenaGameFeature feature = arena.getArenaFeature(GameFeature.class);
            if (feature.getGame() == null) {
                return false;
            }
            if (myArena) {
                return player.getUuid().equals(feature.getOwner());
            } else {
                return true;
            }
        }).toList();
    }
}
