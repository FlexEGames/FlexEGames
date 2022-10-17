package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.duel.DuelGameConfig;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

import java.util.concurrent.TimeUnit;

public class WaitingState implements ComponentGameState {
    private final DuelExtension duelExtension;

    public WaitingState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(ArenaTimerFeature.class)
                .setDuration(
                        arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class).getWaitingTime(),
                        TimeUnit.SECONDS
                );
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS) > 0) return;
        if (arena.getArenaFeature(JoinFeature.class).getPlayerCount() > 1) {
            arena.setNextState(InGameState.class);
        } else {
            Component component = duelExtension.getMessageConfig().getNotEnoughPlayers();
            for (Player player : arena.getArenaFeature(JoinFeature.class).getPlayers()) {
                player.sendMessage(component);
            }
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateWaiting();
    }
}
