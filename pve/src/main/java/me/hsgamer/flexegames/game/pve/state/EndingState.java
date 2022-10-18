package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.flexegames.game.pve.feature.MobGeneratorFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;

import java.util.concurrent.TimeUnit;

public class EndingState implements ComponentGameState {
    private final PveExtension pveExtension;

    public EndingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);
        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
        arena.getArenaFeature(ArenaTimerFeature.class).setDuration(gameConfig.getEndingTime(), TimeUnit.SECONDS);
        arena.getArenaFeature(InstanceFeature.class).clearInventory();
        Component message = ReplacementManager.replace(gameConfig.getEndMessage(), descriptionFeature.getReplacements());
        arena.getArenaFeature(JoinFeature.class).getPlayers().forEach(player -> player.sendMessage(message));
        arena.getArenaFeature(MobGeneratorFeature.class).clearMobs();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS) <= 0) {
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateEnding();
    }
}
