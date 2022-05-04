package me.hsgamer.epicmegagames.state;

import me.hsgamer.epicmegagames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class KillingState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().clear();
        arena.removeFromManager();
    }
}
