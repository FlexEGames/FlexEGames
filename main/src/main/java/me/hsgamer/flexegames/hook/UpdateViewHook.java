package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerSpawnEvent;

/**
 * The hook for updating the view
 */
@UtilityClass
public final class UpdateViewHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerSpawnEvent.class, event -> {
            var player = event.getPlayer();
            var instance = event.getSpawnInstance();
            for (var instancePlayer : instance.getPlayers()) {
                if (instancePlayer == player) {
                    continue;
                }
                MinecraftServer.getSchedulerManager().scheduleNextTick(() -> updateView(instancePlayer));
            }
            MinecraftServer.getSchedulerManager().scheduleNextTick(() -> updateView(player));
        });
    }

    private static void updateView(Player player) {
        player.updateViewerRule();
        player.updateViewableRule();
    }
}
