package me.hsgamer.flexegames.state;

import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class EndingState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().onEndingStart();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(GameFeature.class).getGame().isEndingOver()) {
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().onEndingOver();
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_ENDING.getValue();
    }
}
