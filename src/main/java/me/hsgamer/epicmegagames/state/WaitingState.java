package me.hsgamer.epicmegagames.state;

import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.feature.TemplateFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;

public class WaitingState implements GameState {
    @Override
    public void start(Arena arena) {
        Template template = arena.getArenaFeature(TemplateFeature.class).getTemplate();
        template.postInit();
        template.onWaitingStart();
    }

    @Override
    public void update(Arena arena) {
        Template template = arena.getArenaFeature(TemplateFeature.class).getTemplate();
        if (template.isWaitingEnd()) {
            if (template.canStart()) {
                arena.setNextState(InGameState.class);
            } else {
                arena.setNextState(KillingState.class);
            }
        }
    }

    @Override
    public void end(Arena arena) {
        arena.getArenaFeature(TemplateFeature.class).getTemplate().onWaitingEnd();
    }
}
