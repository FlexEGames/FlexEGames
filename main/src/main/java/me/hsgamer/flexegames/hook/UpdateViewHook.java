package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.timer.ExecutionType;

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
                MinecraftServer.getSchedulerManager().scheduleNextTick(() -> updateView(instancePlayer), ExecutionType.ASYNC);
            }
            MinecraftServer.getSchedulerManager().scheduleNextTick(() -> updateView(player), ExecutionType.ASYNC);
        });
    }

    private static void updateView(Player player) {
        player.updateViewerRule();
        player.updateViewableRule();
    }
}
