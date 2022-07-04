package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.AsyncPlayerPreLoginEvent;
import net.minestom.server.event.player.PlayerDisconnectEvent;
import net.minestom.server.event.player.PlayerLoginEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@UtilityClass
public final class LoginLogHook {
    private static final Logger LOGGER = LoggerFactory.getLogger("Login");

    public static void hook(EventNode<Event> node) {
        node.addListener(AsyncPlayerPreLoginEvent.class, event -> {
            var player = event.getPlayer();
            LOGGER.info("{} ({}) is trying to log in", player.getUsername(), player.getPlayerConnection().getRemoteAddress());
        });
        node.addListener(PlayerLoginEvent.class, event -> {
            var player = event.getPlayer();
            LOGGER.info("{} ({}) logged in", player.getUsername(), player.getPlayerConnection().getRemoteAddress());
        });
        node.addListener(PlayerDisconnectEvent.class, event -> {
            var player = event.getPlayer();
            LOGGER.info("{} ({}) disconnected", player.getUsername(), player.getPlayerConnection().getRemoteAddress());
        });
    }
}
