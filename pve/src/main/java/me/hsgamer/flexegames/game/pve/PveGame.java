package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.GameState;

import java.util.List;

public class PveGame extends Game {
    public static final int SPAWN_RADIUS = 10;
    public static final int HEIGHT = 16;

    public PveGame(Pair<GameServer, Config> pair) {
        super(pair);
    }

    @Override
    protected Class<? extends GameState> getInitialState() {
        return null;
    }

    @Override
    protected List<GameState> loadGameStates() {
        return null;
    }
}
