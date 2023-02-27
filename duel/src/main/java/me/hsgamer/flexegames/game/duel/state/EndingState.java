package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.game.duel.feature.WinnerFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.state.KillingState;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class EndingState implements GameState, ComponentDisplayName {
    private final DuelExtension duelExtension;

    public EndingState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void start(Arena arena) {
        var winnerFeature = arena.getFeature(WinnerFeature.class);
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        arena.getFeature(TimerFeature.class).setDuration(gameConfig.getEndingTime(), TimeUnit.SECONDS);
        Component message;
        if (winnerFeature.getWinner() != null) {
            message = ReplacementManager.replace(duelExtension.getMessageConfig().getWinnerMessage(), descriptionFeature.getReplacements());
        } else {
            message = ReplacementManager.replace(duelExtension.getMessageConfig().getNoWinnerMessage(), descriptionFeature.getReplacements());
        }
        arena.getFeature(InstanceFeature.class).clearInventory();
        arena.getFeature(InstanceFeature.class).sendMessage(message);
    }

    @Override
    public void update(Arena arena) {
        if (arena.getFeature(TimerFeature.class).getDuration() <= 0) {
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateEnding();
    }
}
