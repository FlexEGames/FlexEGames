package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.api.game.GameArena;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.pve.feature.*;
import me.hsgamer.flexegames.game.pve.state.EndingState;
import me.hsgamer.flexegames.game.pve.state.FightingState;
import me.hsgamer.flexegames.game.pve.state.RestingState;
import me.hsgamer.flexegames.game.pve.state.WaitingState;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;

import java.util.List;

public class PveArena extends GameArena<PveGame> {
    private final PveExtension pveExtension;

    public PveArena(PveExtension pveExtension, PveGame game, String name, ArenaManager arenaManager) {
        super(name, game, arenaManager);
        this.pveExtension = pveExtension;
    }

    @Override
    protected DescriptionFeature createDescriptionFeature() {
        return new GameDescriptionFeature(this);
    }

    @Override
    protected JoinFeature createJoinFeature() {
        return new GameJoinFeature(this, pveExtension);
    }

    @Override
    protected List<Feature> loadExtraFeatures() {
        return List.of(
                new TimerFeature(),
                new InstanceFeature(this),
                new StageFeature(),
                new MobGeneratorFeature(this),
                new BoardFeature(this),
                new ConfigFeature(game.getGameConfig())
        );
    }

    @Override
    protected List<GameState> loadGameStates() {
        return List.of(
                new WaitingState(pveExtension),
                new RestingState(pveExtension),
                new FightingState(pveExtension),
                new EndingState(pveExtension)
        );
    }

    @Override
    protected void postInitArena() {
        setNextState(WaitingState.class);
    }
}
