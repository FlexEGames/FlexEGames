package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.feature.ConfigFeature;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class RestingState implements GameState, ComponentDisplayName {
    private final PveExtension pveExtension;

    public RestingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getFeature(TimerFeature.class)
                .setDuration(
                        arena.getFeature(ConfigFeature.class).config().getRestingTime(),
                        TimeUnit.SECONDS
                );
        arena.getFeature(InstanceFeature.class).respawnDeadPlayers();
        arena.getFeature(InstanceFeature.class).tryHealAll();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getFeature(TimerFeature.class).getDuration() > 0) return;
        arena.setNextState(FightingState.class);
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateResting();
    }
}
