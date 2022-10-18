package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class RestingState implements ComponentGameState {
    private final PveExtension pveExtension;

    public RestingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(ArenaTimerFeature.class)
                .setDuration(
                        arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getRestingTime(),
                        TimeUnit.SECONDS
                );
        arena.getArenaFeature(InstanceFeature.class).respawnDeadPlayers();
        arena.getArenaFeature(InstanceFeature.class).tryHealAll();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS) > 0) return;
        arena.setNextState(FightingState.class);
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateResting();
    }
}
