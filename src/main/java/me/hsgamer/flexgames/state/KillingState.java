package me.hsgamer.flexgames.state;

import me.hsgamer.flexgames.config.MessageConfig;
import me.hsgamer.flexgames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.minestom.server.MinecraftServer;

public class KillingState implements GameState {
    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(GameFeature.class).getGame().clear();
        MinecraftServer.getSchedulerManager().scheduleNextTick(arena::removeFromManager);
    }

    @Override
    public String getDisplayName() {
        return MessageConfig.STATE_KILLING.getValue();
    }
}
