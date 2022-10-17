package me.hsgamer.flexegames.game;

import me.hsgamer.flexegames.feature.GameServerFeature;
import me.hsgamer.flexegames.util.TaskUtil;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.MinecraftServer;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;

class GameArena extends Arena {
    private Task task;

    GameArena(String name, Game game) {
        super(name, game);
    }

    @Override
    public void init() {
        var gameServer = getFeature(GameServerFeature.class).gameServer();
        task = MinecraftServer.getSchedulerManager()
                .buildTask(this)
                .repeat(TaskUtil.tick(gameServer.getMainConfig().getArenaPeriod()))
                .executionType(gameServer.getMainConfig().isArenaAsync() ? ExecutionType.ASYNC : ExecutionType.SYNC)
                .schedule();
    }

    @Override
    public void clear() {
        if (task != null) {
            task.cancel();
        }
    }
}
