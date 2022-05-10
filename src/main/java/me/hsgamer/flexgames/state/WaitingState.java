package me.hsgamer.flexgames.state;

import me.hsgamer.flexgames.api.ArenaGame;
import me.hsgamer.flexgames.config.MessageConfig;
import me.hsgamer.flexgames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class WaitingState implements GameState {
    @Override
    public void start(Arena arena) {
        ArenaGame arenaGame = arena.getArenaFeature(GameFeature.class).getGame();
        arenaGame.postInit();
        arenaGame.onWaitingStart();
    }

    @Override
    public void update(Arena arena) {
        ArenaGame arenaGame = arena.getArenaFeature(GameFeature.class).getGame();
        if (arenaGame.isWaitingOver()) {
            if (arenaGame.canStart()) {
                arena.setNextState(InGameState.class);
            } else {
                arena.setNextState(KillingState.class);
            }
        }
    }

    @Override
    public void end(Arena arena) {
        ArenaGame arenaGame = arena.getArenaFeature(GameFeature.class).getGame();
        if (arenaGame.canStart()) {
            arenaGame.onWaitingEnd();
        } else {
            arenaGame.onFailedWaitingEnd();
        }
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_WAITING.getValue();
    }
}
