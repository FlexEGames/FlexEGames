package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.flexegames.game.pve.feature.MobGeneratorFeature;
import me.hsgamer.flexegames.game.pve.feature.StageFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;

public class FightingState implements ComponentGameState {
    private final PveExtension pveExtension;

    public FightingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getArenaFeature(MobGeneratorFeature.class).loadMobs();
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);
        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
        Component message = ReplacementManager.replace(gameConfig.getStageStartMessage(), descriptionFeature.getReplacements());
        arena.getArenaFeature(JoinFeature.class).getPlayers().forEach(player -> player.sendMessage(message));
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(InstanceFeature.class).isAllDead()) {
            arena.setNextState(EndingState.class);
            return;
        }

        var mobGeneratorFeature = arena.getArenaFeature(MobGeneratorFeature.class);
        if (!mobGeneratorFeature.isCleared()) {
            mobGeneratorFeature.trySpawnMobs();
            return;
        }

        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);
        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
        Component message = ReplacementManager.replace(gameConfig.getStageEndMessage(), descriptionFeature.getReplacements());
        arena.getArenaFeature(JoinFeature.class).getPlayers().forEach(player -> player.sendMessage(message));

        arena.getArenaFeature(StageFeature.class).increaseStage();
        arena.setNextState(RestingState.class);
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateFighting();
    }
}
