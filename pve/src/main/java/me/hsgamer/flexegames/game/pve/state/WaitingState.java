package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.feature.ConfigFeature;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.flexegames.game.pve.feature.StageFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.state.KillingState;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class WaitingState implements GameState, ComponentDisplayName {
    private final PveExtension pveExtension;

    public WaitingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
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
        if (arena.getFeature(TimerFeature.class).getDuration(TimeUnit.MILLISECONDS) > 0) return;
        if (arena.getFeature(JoinFeature.class).getPlayerCount() >= arena.getFeature(ConfigFeature.class).config().getMinPlayers()) {
            arena.setNextState(RestingState.class);
        } else {
            Component message = pveExtension.getMessageConfig().getNotEnoughPlayers();
            arena.getFeature(InstanceFeature.class).sendMessage(message);
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getFeature(StageFeature.class).setStage(1);
        var gameConfig = arena.getFeature(ConfigFeature.class).config();
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        Component message = ReplacementManager.replace(gameConfig.getStartMessage(), descriptionFeature.getReplacements());
        arena.getFeature(InstanceFeature.class).sendMessage(message);
        arena.getFeature(InstanceFeature.class).giveKit();
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateWaiting();
    }
}
