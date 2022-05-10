package me.hsgamer.flexgames.arena;

import me.hsgamer.flexgames.config.MainConfig;
import me.hsgamer.flexgames.state.ChoosingState;
import me.hsgamer.flexgames.util.TaskUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.minestom.server.MinecraftServer;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;

public class GameArena extends Arena {
    private Task task;

    public GameArena(String name, ArenaManager arenaManager) {
        super(name, arenaManager);
        setNextState(ChoosingState.class);
    }

    @Override
    public void init() {
        task = MinecraftServer.getSchedulerManager()
                .buildTask(this)
                .repeat(TaskUtil.tick(MainConfig.ARENA_PERIOD.getValue()))
                .executionType(Boolean.TRUE.equals(MainConfig.ARENA_ASYNC.getValue()) ? ExecutionType.ASYNC : ExecutionType.SYNC)
                .schedule();
    }

    @Override
    public void clear() {
        if (task != null) {
            task.cancel();
        }
    }
}
