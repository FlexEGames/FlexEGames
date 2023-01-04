package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.game.GameArena;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
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

public class DuelArena extends GameArena<DuelGame> {
    private final DuelExtension duelExtension;

    public DuelArena(DuelExtension duelExtension, DuelGame duelGame, String name, ArenaManager arenaManager) {
        super(name, duelGame, arenaManager);
        this.duelExtension = duelExtension;
    }

    @Override
    protected DescriptionFeature createDescriptionFeature() {
        return new GameDescriptionFeature(this);
    }

    @Override
    protected JoinFeature createJoinFeature() {
        return new GameJoinFeature(this, duelExtension);
    }

    @Override
    protected List<Unit<Feature>> loadExtraFeatures() {
        return List.of(
                new Unit<>(new InstanceFeature(this)),
                new Unit<>(new WinnerFeature(this)),
                new Unit<>(new TimerFeature()),
                new Unit<>(new ConfigFeature(game.getGameConfig()))
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

    @Override
    protected void postInitArena() {
        setNextState(WaitingState.class);
    }
}
