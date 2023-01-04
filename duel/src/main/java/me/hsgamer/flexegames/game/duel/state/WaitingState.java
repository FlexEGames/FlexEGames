package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.feature.ConfigFeature;
import me.hsgamer.flexegames.game.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.state.KillingState;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class WaitingState implements GameState, ComponentDisplayName {
    private final DuelExtension duelExtension;

    public WaitingState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getFeature(TimerFeature.class)
                .setDuration(
                        arena.getFeature(ConfigFeature.class).config().getWaitingTime(),
                        TimeUnit.SECONDS
                );
    }

    @Override
    public void update(Arena arena) {
        if (arena.getFeature(TimerFeature.class).getDuration() > 0) return;
        if (arena.getFeature(JoinFeature.class).getPlayerCount() > 1) {
            arena.setNextState(InGameState.class);
        } else {
            Component component = duelExtension.getMessageConfig().getNotEnoughPlayers();
            arena.getFeature(InstanceFeature.class).sendMessage(component);
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateWaiting();
    }
}
