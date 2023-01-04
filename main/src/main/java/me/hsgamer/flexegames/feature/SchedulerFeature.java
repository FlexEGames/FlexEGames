package me.hsgamer.flexegames.feature;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Feature;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The feature to schedule tasks
 */
public class SchedulerFeature implements Feature {
    private final ScheduledThreadPoolExecutor executor;

    public SchedulerFeature(GameServer gameServer) {
        this.executor = new ScheduledThreadPoolExecutor(gameServer.getMainConfig().getSchedulerSize());
        executor.setRemoveOnCancelPolicy(true);
    }

    /**
     * Schedule a task
     *
     * @param runnable the task
     * @param period   the period
     * @return the {@link ScheduledFuture}
     */
    public ScheduledFuture<?> schedule(Runnable runnable, long period) {
        return executor.scheduleAtFixedRate(runnable, 0, period, TimeUnit.MILLISECONDS);
    }

    /**
     * Schedule a task
     *
     * @param runnable the task
     * @param delay    the delay
     * @param period   the period
     * @return the {@link ScheduledFuture}
     */
    public ScheduledFuture<?> schedule(Runnable runnable, long delay, long period) {
        return executor.scheduleAtFixedRate(runnable, delay, period, TimeUnit.MILLISECONDS);
    }

    /**
     * Schedule a task
     *
     * @param runnable the task
     * @return the {@link ScheduledFuture}
     */
    public ScheduledFuture<?> schedule(Runnable runnable) {
        return executor.scheduleWithFixedDelay(runnable, 0, 1, TimeUnit.MILLISECONDS);
    }

    @Override
    public void clear() {
        executor.shutdown();
    }
}
