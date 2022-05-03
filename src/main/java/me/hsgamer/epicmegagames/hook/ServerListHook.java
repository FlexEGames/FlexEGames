package me.hsgamer.epicmegagames.hook;

import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.server.ServerListPingEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.network.player.PlayerConnection;
import net.minestom.server.ping.ResponseData;

import java.util.Collection;
import java.util.Optional;

public class ServerListHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(ServerListPingEvent.class, event -> {
            ResponseData responseData = event.getResponseData();
            Optional<Instance> optionalInstance = Optional.ofNullable(event.getConnection()).map(PlayerConnection::getPlayer).map(Entity::getInstance);
            Collection<Player> players;
            if (optionalInstance.isPresent()) {
                Instance instance = optionalInstance.get();
                players = instance.getPlayers();
            } else {
                players = MinecraftServer.getConnectionManager().getOnlinePlayers();
            }
            responseData.addEntries(players);
            responseData.setMaxPlayer(players.size() + 1);
        });
    }
}
