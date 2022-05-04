package me.hsgamer.epicmegagames.state;

import me.hsgamer.epicmegagames.feature.TemplateFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class KillingState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(TemplateFeature.class).getTemplate().clear();
        arena.removeFromManager();
    }
}
