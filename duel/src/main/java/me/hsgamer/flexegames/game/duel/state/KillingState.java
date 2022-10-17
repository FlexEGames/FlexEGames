package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;

public class KillingState implements ComponentGameState {
    private final DuelExtension duelExtension;

    public KillingState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void update(Arena arena) {
        arena.removeFromManager();
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateKilling();
    }
}
