package me.hsgamer.epicmegagames.arena;

import me.hsgamer.epicmegagames.state.ChoosingState;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.minestom.server.MinecraftServer;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

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
                .repeat(TaskSchedule.tick(20))
                .executionType(ExecutionType.SYNC)
                .schedule();
    }

    @Override
    public void clear() {
        if (task != null) {
            task.cancel();
        }
    }
}
