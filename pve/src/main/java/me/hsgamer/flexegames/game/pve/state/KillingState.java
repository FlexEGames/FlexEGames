package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;

public class KillingState implements ComponentGameState {
    private final PveExtension pveExtension;

    public KillingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateKilling();
    }

    @Override
    public void update(Arena arena) {
        arena.removeFromManager();
    }
}
