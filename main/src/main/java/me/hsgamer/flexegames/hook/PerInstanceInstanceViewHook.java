package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.entity.PlayerSkin;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.network.packet.server.play.PlayerInfoPacket;
import net.minestom.server.tag.Tag;
import net.minestom.server.utils.PacketUtils;

import java.util.List;

/**
 * The hook for per-instance instance view (tab list view)
 */
@UtilityClass
public class PerInstanceInstanceViewHook {
    private static final Tag<Boolean> firstSpawnTag = Tag.Boolean("InstanceViewHook:FirstSpawn").defaultValue(true);

    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerSpawnEvent.class, event -> {
            var player = event.getPlayer();
            var instance = event.getSpawnInstance();
            PacketUtils.sendGroupedPacket(instance.getPlayers(), getAddPlayerPacket(player), player1 -> player1 != player);
            if (Boolean.TRUE.equals(player.getTag(firstSpawnTag))) { // On First Spawn
                for (Player onlinePlayer : MinecraftServer.getConnectionManager().getOnlinePlayers()) {
                    if (onlinePlayer == player || onlinePlayer.getInstance() == instance) continue;
                    PacketUtils.sendPacket(player, getRemovePlayerPacket(onlinePlayer));
                    PacketUtils.sendPacket(onlinePlayer, getAddPlayerPacket(player));
                }
                player.setTag(firstSpawnTag, false);
            } else { // On Switch Instance
                for (Player instancePlayer : instance.getPlayers()) {
                    if (instancePlayer == player) continue;
                    PacketUtils.sendPacket(player, getAddPlayerPacket(instancePlayer));
                }
            }
        }).addListener(RemoveEntityFromInstanceEvent.class, event -> {
            if (!(event.getEntity() instanceof Player player)) return;
            Instance instance = event.getInstance();
            PacketUtils.sendGroupedPacket(instance.getPlayers(), getRemovePlayerPacket(player), player1 -> player1 != player);
            for (Player instancePlayer : instance.getPlayers()) {
                if (instancePlayer == player) continue;
                PacketUtils.sendPacket(player, getRemovePlayerPacket(instancePlayer));
            }
        });
    }

    private static PlayerInfoPacket getAddPlayerPacket(Player player) {
        final PlayerSkin skin = player.getSkin();
        List<PlayerInfoPacket.AddPlayer.Property> prop = skin != null ?
                List.of(new PlayerInfoPacket.AddPlayer.Property("textures", skin.textures(), skin.signature())) :
                List.of();
        return new PlayerInfoPacket(PlayerInfoPacket.Action.ADD_PLAYER,
                new PlayerInfoPacket.AddPlayer(player.getUuid(), player.getUsername(), prop, player.getGameMode(), player.getLatency(), player.getDisplayName(), null));
    }

    private static PlayerInfoPacket getRemovePlayerPacket(Player player) {
        return new PlayerInfoPacket(PlayerInfoPacket.Action.REMOVE_PLAYER, new PlayerInfoPacket.RemovePlayer(player.getUuid()));
    }
}
