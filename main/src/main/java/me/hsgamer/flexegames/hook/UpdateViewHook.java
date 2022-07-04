package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.TaskSchedule;

@UtilityClass
public final class UpdateViewHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().scheduler()
                .buildTask(() -> {
                    event.getPlayer().updateViewableRule();
                    event.getPlayer().updateViewerRule();
                    event.getPlayer().askSynchronization();
                })
                .delay(TaskSchedule.tick(20))
                .repeat(TaskSchedule.tick(20))
                .executionType(ExecutionType.ASYNC)
                .schedule());
    }
}
