package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.game.GameArena;
import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.duel.feature.GameDescriptionFeature;
import me.hsgamer.flexegames.game.duel.feature.GameJoinFeature;
import me.hsgamer.flexegames.game.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.game.duel.feature.WinnerFeature;
import me.hsgamer.flexegames.game.duel.kit.DuelKit;
import me.hsgamer.flexegames.game.duel.state.EndingState;
import me.hsgamer.flexegames.game.duel.state.InGameState;
import me.hsgamer.flexegames.game.duel.state.WaitingState;
import me.hsgamer.flexegames.game.duel.world.DuelWorld;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;

import java.util.List;
import java.util.UUID;

public class DuelArena extends GameArena<DuelGame> {
    private final DuelExtension duelExtension;
    private final DuelWorld duelWorld;
    private final DuelKit duelKit;

    public DuelArena(DuelExtension duelExtension, String name, GamePropertyMap propertyMap, DuelGame game, ArenaManager arenaManager, UUID owner) {
        super(name, propertyMap, game, arenaManager, owner);
        this.duelExtension = duelExtension;
        this.duelWorld = duelExtension.getDuelWorldManager().getDuelWorld(propertyMap.getProperty(DuelProperties.WORLD));
        this.duelKit = duelExtension.getDuelKitManager().getDuelKit(propertyMap.getProperty(DuelProperties.KIT));
    }

    @Override
    protected DescriptionFeature createDescriptionFeature() {
        return new GameDescriptionFeature(this, duelExtension);
    }

    @Override
    protected JoinFeature createJoinFeature() {
        return new GameJoinFeature(this, duelExtension);
    }

    @Override
    protected List<Feature> loadExtraFeatures() {
        return List.of(
                new InstanceFeature(this, duelWorld, duelKit, duelExtension),
                new WinnerFeature(this),
                new TimerFeature()
        );
    }

    @Override
    protected List<GameState> loadGameStates() {
        return List.of(
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
