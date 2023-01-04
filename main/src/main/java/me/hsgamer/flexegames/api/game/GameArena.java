package me.hsgamer.flexegames.api.game;

import me.hsgamer.flexegames.feature.SchedulerFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.feature.arena.OwnerFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.Unit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ScheduledFuture;

/**
 * The arena for the game
 */
public abstract class GameArena<T extends Game> extends Arena {
    protected final T game;
    private ScheduledFuture<?> task;

    protected GameArena(String name, T game, ArenaManager arenaManager) {
        super(name, arenaManager);
        this.game = game;
    }

    @Override
    public void initArena() {
        task = getFeature(SchedulerFeature.class).schedule(this);
    }

    /**
     * Load the extra features
     *
     * @return the list of features
     */
    protected List<Unit<Feature>> loadExtraFeatures() {
        return Collections.emptyList();
    }

    /**
     * Create the description feature
     *
     * @return the description feature
     */
    protected abstract DescriptionFeature createDescriptionFeature();

    /**
     * Create the join feature
     *
     * @return the join feature
     */
    protected abstract JoinFeature createJoinFeature();

    @Override
    protected final List<Unit<Feature>> loadFeatures() {
        List<Unit<Feature>> features = new ArrayList<>(loadExtraFeatures());
        features.addAll(List.of(
                new Unit<>(new GameFeature(game)),
                new Unit<>(new OwnerFeature()),
                new Unit<>(DescriptionFeature.class, createDescriptionFeature()),
                new Unit<>(JoinFeature.class, createJoinFeature())
        ));
        return features;
    }

    @Override
    public void clearArena() {
        if (task != null) {
            task.cancel(true);
        }
    }
}
