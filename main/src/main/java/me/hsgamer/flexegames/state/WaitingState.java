package me.hsgamer.flexegames.state;

import me.hsgamer.flexegames.api.game.ArenaGame;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
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
        var gameFeature = arena.getArenaFeature(GameFeature.class);
        var arenaGame = gameFeature.getGame();
        if (arenaGame.isWaitingOver()) {
            if (arenaGame.canStart()) {
                gameFeature.setFailed(false);
                arena.setNextState(InGameState.class);
            } else {
                gameFeature.setFailed(true);
                arena.setNextState(KillingState.class);
            }
        } else {
            arenaGame.onWaitingTick();
        }
    }

    @Override
    public void end(Arena arena) {
        var gameFeature = arena.getArenaFeature(GameFeature.class);
        var arenaGame = gameFeature.getGame();
        if (gameFeature.isFailed()) {
            arenaGame.onFailedWaitingEnd();
        } else {
            arenaGame.onWaitingEnd();
        }
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_WAITING.getValue();
    }
}
