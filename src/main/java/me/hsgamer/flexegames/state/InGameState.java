package me.hsgamer.flexegames.state;

import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class InGameState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().onInGameStart();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(GameFeature.class).getGame().isInGameOver()) {
            arena.setNextState(EndingState.class);
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().onInGameOver();
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_IN_GAME.getValue();
    }
}
