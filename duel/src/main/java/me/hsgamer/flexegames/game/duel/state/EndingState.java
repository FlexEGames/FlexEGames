package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.DuelGameConfig;
import me.hsgamer.flexegames.game.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.game.duel.feature.WinnerFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class EndingState implements ComponentGameState {
    private final DuelExtension duelExtension;

    public EndingState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void start(Arena arena) {
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class);
        var winnerFeature = arena.getArenaFeature(WinnerFeature.class);
        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
        arena.getArenaFeature(ArenaTimerFeature.class).setDuration(gameConfig.getEndingTime(), TimeUnit.SECONDS);
        Component message;
        if (winnerFeature.getWinner() != null) {
            message = ReplacementManager.replace(gameConfig.getWinnerMessage(), descriptionFeature.getReplacements());
        } else {
            message = gameConfig.getNoWinnerMessage();
        }
        arena.getArenaFeature(InstanceFeature.class).clearInventory();
        arena.getArenaFeature(InstanceFeature.class).sendMessage(message);
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS) <= 0) {
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateEnding();
    }
}
