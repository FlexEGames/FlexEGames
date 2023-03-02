package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.flexegames.game.pve.feature.MobGeneratorFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.state.KillingState;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class EndingState implements GameState, ComponentDisplayName {
    private final PveExtension pveExtension;

    public EndingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        arena.getFeature(TimerFeature.class).setDuration(pveExtension.getMainConfig().getEndingTime(), TimeUnit.SECONDS);
        arena.getFeature(InstanceFeature.class).clearKit();
        Component message = ReplacementManager.replace(pveExtension.getMessageConfig().getEndMessage(), descriptionFeature.getReplacements());
        arena.getFeature(InstanceFeature.class).sendMessage(message);
        arena.getFeature(MobGeneratorFeature.class).clearMobs();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getFeature(TimerFeature.class).getDuration() <= 0) {
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateEnding();
    }
}
