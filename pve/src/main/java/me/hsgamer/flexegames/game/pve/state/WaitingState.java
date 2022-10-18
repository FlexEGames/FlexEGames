package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.feature.StageFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

import java.util.concurrent.TimeUnit;

public class WaitingState implements ComponentGameState {
    private final PveExtension pveExtension;

    public WaitingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(ArenaTimerFeature.class)
                .setDuration(
                        arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getWaitingTime(),
                        TimeUnit.SECONDS
                );
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS) > 0) return;
        if (arena.getArenaFeature(JoinFeature.class).getPlayerCount() >= arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getMinPlayers()) {
            arena.setNextState(RestingState.class);
        } else {
            Component component = pveExtension.getMessageConfig().getNotEnoughPlayers();
            for (Player player : arena.getArenaFeature(JoinFeature.class).getPlayers()) {
                player.sendMessage(component);
            }
            arena.setNextState(KillingState.class);
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getArenaFeature(StageFeature.class).setStage(1);
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);
        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
        Component message = ReplacementManager.replace(gameConfig.getStartMessage(), descriptionFeature.getReplacements());
        arena.getArenaFeature(JoinFeature.class).getPlayers().forEach(player -> player.sendMessage(message));
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateWaiting();
    }
}
