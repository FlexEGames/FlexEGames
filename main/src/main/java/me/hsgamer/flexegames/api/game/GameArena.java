package me.hsgamer.flexegames.api.game;

import me.hsgamer.flexegames.api.property.PropertyMap;
import me.hsgamer.flexegames.feature.GameServerFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.feature.arena.OwnerFeature;
import me.hsgamer.flexegames.util.TaskUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.MinecraftServer;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The {@link Arena} of the {@link Game}
 */
public abstract class GameArena<T extends Game> extends Arena {
    /**
     * The property map of the arena
     */
    protected final PropertyMap propertyMap;
    /**
     * The game
     */
    protected final T game;
    private final UUID owner;
    private Task task;

    /**
     * Create a new arena
     *
     * @param name         the name of the arena
     * @param propertyMap  the property map
     * @param game         the game
     * @param arenaManager the arena manager
     * @param owner        the owner
     */
    protected GameArena(String name, PropertyMap propertyMap, T game, ArenaManager arenaManager, UUID owner) {
        super(name, arenaManager);
        this.propertyMap = propertyMap;
        this.game = game;
        this.owner = owner;
    }

    @Override
    public void initArena() {
        var gameServer = getFeature(GameServerFeature.class).gameServer();
        task = MinecraftServer.getSchedulerManager()
                .buildTask(this)
                .repeat(TaskUtil.tick(gameServer.getMainConfig().getArenaPeriod()))
                .executionType(gameServer.getMainConfig().isArenaAsync() ? ExecutionType.ASYNC : ExecutionType.SYNC)
                .schedule();
    }

    /**
     * Load the extra features
     *
     * @return the list of features
     */
    protected List<Feature> loadExtraFeatures() {
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
    protected final List<Feature> loadFeatures() {
        List<Feature> features = new ArrayList<>(List.of(
                new GameFeature(game, propertyMap),
                new OwnerFeature(owner),
                createDescriptionFeature(),
                createJoinFeature()
        ));
        features.addAll(loadExtraFeatures());
        return features;
    }

    @Override
    public void clearArena() {
        if (task != null) {
            task.cancel();
        }
    }
}
