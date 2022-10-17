package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.minigamecore.base.Arena;

public class KillingState implements ComponentGameState {
    @Override
    public void update(Arena arena) {
        arena.removeFromManager();
    }
}
