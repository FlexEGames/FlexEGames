package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.entity.PlayerSkin;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.network.packet.server.play.PlayerInfoPacket;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.TaskSchedule;

import java.util.*;

@UtilityClass
public final class PerInstanceTabListHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().scheduler()
                .buildTask(new TabPlayerRunnable(event.getPlayer()))
                .delay(TaskSchedule.tick(10))
                .repeat(TaskSchedule.tick(10))
                .executionType(ExecutionType.ASYNC)
                .schedule());
    }

    private static PlayerInfoPacket getAddPlayerPacket(Player player) {
        final PlayerSkin skin = player.getSkin();
        List<PlayerInfoPacket.AddPlayer.Property> prop = skin != null ?
                List.of(new PlayerInfoPacket.AddPlayer.Property("textures", skin.textures(), skin.signature())) :
                Collections.emptyList();
        return new PlayerInfoPacket(PlayerInfoPacket.Action.ADD_PLAYER,
                new PlayerInfoPacket.AddPlayer(player.getUuid(), player.getUsername(), prop, player.getGameMode(), player.getLatency(), player.getDisplayName()));
    }

    private static PlayerInfoPacket getRemovePlayerPacket(Player player) {
        return new PlayerInfoPacket(PlayerInfoPacket.Action.REMOVE_PLAYER, new PlayerInfoPacket.RemovePlayer(player.getUuid()));
    }

    private static class TabPlayerRunnable implements Runnable {
        private final Player owner;
        private final Set<UUID> canSee = new HashSet<>();
        private final Set<UUID> cannotSee = new HashSet<>();

        private TabPlayerRunnable(Player owner) {
            this.owner = owner;
        }

        private boolean isSameInstance(Player player) {
            return owner.getInstance() == player.getInstance();
        }

        @Override
        public void run() {
            for (Player player : MinecraftServer.getConnectionManager().getOnlinePlayers()) {
                UUID uuid = player.getUuid();
                if (canSee.contains(uuid)) {
                    if (!isSameInstance(player)) {
                        canSee.remove(uuid);
                    }
                } else if (cannotSee.contains(uuid)) {
                    if (isSameInstance(player)) {
                        cannotSee.remove(uuid);
                    }
                } else {
                    if (isSameInstance(player)) {
                        canSee.add(uuid);
                        owner.sendPacket(getAddPlayerPacket(player));
                    } else {
                        cannotSee.add(uuid);
                        owner.sendPacket(getRemovePlayerPacket(player));
                    }
                }
            }
        }
    }
}
