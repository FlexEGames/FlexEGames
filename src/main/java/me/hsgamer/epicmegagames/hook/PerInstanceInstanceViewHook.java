package me.hsgamer.epicmegagames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.entity.PlayerSkin;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.network.packet.server.play.PlayerInfoPacket;
import net.minestom.server.utils.PacketUtils;

import java.util.Collections;
import java.util.List;

@UtilityClass
public final class PerInstanceInstanceViewHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(AddEntityToInstanceEvent.class, event -> {
            if (!(event.getEntity() instanceof Player player)) return;
            Instance instance = event.getInstance();
            PacketUtils.sendGroupedPacket(instance.getPlayers(), getAddPlayerPacket(player), player1 -> player1 != player);
            if (player.getInstance() == null) { // On First Spawn
                for (Player onlinePlayer : MinecraftServer.getConnectionManager().getOnlinePlayers()) {
                    if (onlinePlayer == player || onlinePlayer.getInstance() == instance) continue;
                    PacketUtils.sendPacket(player, getRemovePlayerPacket(onlinePlayer));
                    PacketUtils.sendPacket(onlinePlayer, getAddPlayerPacket(player));
                }
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
                Collections.emptyList();
        return new PlayerInfoPacket(PlayerInfoPacket.Action.ADD_PLAYER,
                new PlayerInfoPacket.AddPlayer(player.getUuid(), player.getUsername(), prop, player.getGameMode(), player.getLatency(), player.getDisplayName()));
    }

    private static PlayerInfoPacket getRemovePlayerPacket(Player player) {
        return new PlayerInfoPacket(PlayerInfoPacket.Action.REMOVE_PLAYER, new PlayerInfoPacket.RemovePlayer(player.getUuid()));
    }
}
