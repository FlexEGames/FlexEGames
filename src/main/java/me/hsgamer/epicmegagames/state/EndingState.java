package me.hsgamer.epicmegagames.state;

import me.hsgamer.epicmegagames.feature.TemplateFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class EndingState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(TemplateFeature.class).getTemplate().onEndingStart();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(TemplateFeature.class).getTemplate().isEndingOver()) {
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getArenaFeature(TemplateFeature.class).getTemplate().onEndingOver();
    }
}
