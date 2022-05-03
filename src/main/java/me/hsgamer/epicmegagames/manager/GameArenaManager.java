package me.hsgamer.epicmegagames.manager;

import me.hsgamer.epicmegagames.feature.PlayerFeature;
import me.hsgamer.epicmegagames.feature.TemplateFeature;
import me.hsgamer.epicmegagames.state.*;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;

import java.util.List;

public class GameArenaManager extends ArenaManager {
    @Override
    protected List<GameState> loadGameStates() {
        return List.of(
                new ChoosingState(),
                new WaitingState(),
                new InGameState(),
                new EndingState(),
                new KillingState()
        );
    }

    @Override
    protected List<Feature> loadFeatures() {
        return List.of(
                new PlayerFeature(),
                new TemplateFeature()
        );
    }
}
