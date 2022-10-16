package me.hsgamer.flexegames.game;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.feature.GameServerFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;

import java.util.ArrayList;
import java.util.List;

public abstract class Game extends ArenaManager {
    protected final GameServer gameServer;

    protected Game(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    @Override
    protected List<Feature> loadFeatures() {
        List<Feature> features = new ArrayList<>(getFeatures());
        features.add(new LobbyFeature(gameServer));
        features.add(new GameServerFeature(gameServer));
        return features;
    }

    protected abstract List<Feature> getFeatures();

    protected abstract Class<? extends GameState> getInitialState();
}
