package me.hsgamer.flexgames.state;

import me.hsgamer.flexgames.config.MessageConfig;
import me.hsgamer.flexgames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class ChoosingState implements GameState {
    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(GameFeature.class).getGame() != null) {
            arena.setNextState(WaitingState.class);
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().init();
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_CHOOSING.getValue();
    }
}
