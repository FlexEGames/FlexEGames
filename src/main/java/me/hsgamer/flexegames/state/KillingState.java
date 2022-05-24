package me.hsgamer.flexegames.state;

import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.minestom.server.MinecraftServer;

public class KillingState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().clear();
    }

    @Override
    public void update(Arena arena) {
        arena.removeFromManager();
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_KILLING.getValue();
    }
}
