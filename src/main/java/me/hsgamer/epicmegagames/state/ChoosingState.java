package me.hsgamer.epicmegagames.state;

import me.hsgamer.epicmegagames.feature.TemplateFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class ChoosingState implements GameState {
    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(TemplateFeature.class).getTemplate() != null) {
            arena.setNextState(WaitingState.class);
        }
    }
}
