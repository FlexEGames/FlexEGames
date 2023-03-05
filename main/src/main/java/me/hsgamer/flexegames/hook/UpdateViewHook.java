package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerLoginEvent;
import net.minestom.server.timer.TaskSchedule;

/**
 * The hook for updating the view
 */
@UtilityClass
public final class UpdateViewHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerLoginEvent.class, event -> {
            var player = event.getPlayer();
            player.scheduler().scheduleTask(() -> updateView(player), TaskSchedule.immediate(), TaskSchedule.nextTick());
        });
    }

    private static void updateView(Player player) {
        player.updateViewerRule();
        player.updateViewableRule();
    }
}
