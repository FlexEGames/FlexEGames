package me.hsgamer.flexegames.game.pve.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.feature.ConfigFeature;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.flexegames.game.pve.feature.MobGeneratorFeature;
import me.hsgamer.flexegames.game.pve.feature.StageFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;

public class FightingState implements GameState, ComponentDisplayName {
    private final PveExtension pveExtension;

    public FightingState(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public void start(Arena arena) {
        arena.getFeature(MobGeneratorFeature.class).loadMobs();
        var gameConfig = arena.getFeature(ConfigFeature.class).config();
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        Component message = ReplacementManager.replace(gameConfig.getStageStartMessage(), descriptionFeature.getReplacements());
        arena.getFeature(InstanceFeature.class).sendMessage(message);
    }

    @Override
    public void update(Arena arena) {
        if (arena.getFeature(InstanceFeature.class).isAllDead()) {
            arena.setNextState(EndingState.class);
            return;
        }

        var mobGeneratorFeature = arena.getFeature(MobGeneratorFeature.class);
        if (!mobGeneratorFeature.isCleared()) {
            mobGeneratorFeature.trySpawnMobs();
            return;
        }

        var gameConfig = arena.getFeature(ConfigFeature.class).config();
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        Component message = ReplacementManager.replace(gameConfig.getStageEndMessage(), descriptionFeature.getReplacements());
        arena.getFeature(InstanceFeature.class).sendMessage(message);

        arena.getFeature(StageFeature.class).increaseStage();
        arena.setNextState(RestingState.class);
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return pveExtension.getMessageConfig().getStateFighting();
    }
}
