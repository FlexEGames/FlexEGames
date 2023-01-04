package me.hsgamer.flexegames.api.game;

import me.hsgamer.flexegames.feature.GameServerFeature;
import me.hsgamer.flexegames.util.TaskUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.minestom.server.MinecraftServer;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;

/**
 * The arena for the game
 */
public class GameArena extends Arena {
    private Task task;

    public GameArena(String name, ArenaManager arenaManager) {
        super(name, arenaManager);
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

    @Override
    public void clearArena() {
        if (task != null) {
            task.cancel();
        }
    }
}
