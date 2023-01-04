package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.game.GameArena;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.feature.arena.OwnerFeature;
import me.hsgamer.flexegames.game.duel.feature.*;
import me.hsgamer.flexegames.game.duel.state.EndingState;
import me.hsgamer.flexegames.game.duel.state.InGameState;
import me.hsgamer.flexegames.game.duel.state.WaitingState;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.base.Unit;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;

import java.util.List;

public class DuelArena extends GameArena {
    private final DuelExtension duelExtension;
    private final DuelGame game;

    public DuelArena(DuelExtension duelExtension, DuelGame game, String name, ArenaManager arenaManager) {
        super(name, arenaManager);
        this.duelExtension = duelExtension;
        this.game = game;
    }

    @Override
    protected List<Unit<Feature>> loadFeatures() {
        return List.of(
                new Unit<>(new InstanceFeature(this)),
                new Unit<>(new WinnerFeature(this)),
                new Unit<>(new TimerFeature()),
                new Unit<>(new ConfigFeature(game.getGameConfig())),
                new Unit<>(new GameFeature(game)),
                new Unit<>(new OwnerFeature()),
                new Unit<>(DescriptionFeature.class, new GameDescriptionFeature(this)),
                new Unit<>(JoinFeature.class, new GameJoinFeature(this, duelExtension))
        );
    }

    @Override
    protected List<Unit<GameState>> loadGameStates() {
        return Unit.wrap(
                new WaitingState(duelExtension),
                new InGameState(duelExtension),
                new EndingState(duelExtension)
        );
    }
}
