package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.config.MainConfig;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.server.ServerListPingEvent;
import net.minestom.server.ping.ResponseData;

import java.util.Collection;

@UtilityClass
public final class ServerListHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(ServerListPingEvent.class, event -> {
            ResponseData responseData = event.getResponseData();
            Collection<Player> players = MinecraftServer.getConnectionManager().getOnlinePlayers();
            if (Boolean.TRUE.equals(MainConfig.SERVER_SHOW_PLAYERS.getValue())) {
                responseData.addEntries(players);
            }
            responseData.setOnline(players.size());
            responseData.setMaxPlayer(players.size() + 1);
        });
    }
}
