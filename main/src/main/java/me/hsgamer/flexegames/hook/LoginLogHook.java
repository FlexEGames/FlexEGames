package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.AsyncPlayerPreLoginEvent;
import net.minestom.server.event.player.PlayerDisconnectEvent;
import net.minestom.server.event.player.PlayerLoginEvent;

import java.util.logging.Logger;

@UtilityClass
public final class LoginLogHook {
    private static final Logger LOGGER = Logger.getLogger("Login");

    public static void hook(EventNode<Event> node) {
        node.addListener(AsyncPlayerPreLoginEvent.class, event -> {
            var player = event.getPlayer();
            LOGGER.info(player.getUsername() + " (" + player.getPlayerConnection().getRemoteAddress() + ")" + " is trying to log in");
        });
        node.addListener(PlayerLoginEvent.class, event -> {
            var player = event.getPlayer();
            LOGGER.info(player.getUsername() + " (" + player.getPlayerConnection().getRemoteAddress() + ")" + " logged in");
        });
        node.addListener(PlayerDisconnectEvent.class, event -> {
            var player = event.getPlayer();
            LOGGER.info(player.getUsername() + " (" + player.getPlayerConnection().getRemoteAddress() + ")" + " disconnected");
        });
    }
}
