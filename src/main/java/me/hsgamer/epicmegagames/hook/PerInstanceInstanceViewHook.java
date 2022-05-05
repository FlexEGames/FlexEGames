package me.hsgamer.epicmegagames.hook;

import net.minestom.server.entity.Entity;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.instance.Instance;

import java.util.function.Predicate;

public class PerInstanceInstanceViewHook {
    public static void hook(EventNode<Event> node) {
//        node.addListener(PlayerTickEvent.class, event -> {
//            Player player = event.getPlayer();
//            player.updateViewableRule(perInstancePlayerPredicate(player.getInstance()));
//            player.updateViewerRule(perInstancePredicate(player.getInstance()));
//        });
    }

    private static Predicate<Entity> perInstancePredicate(Instance instance) {
        return entity -> entity.getInstance() == instance;
    }

    private static Predicate<Player> perInstancePlayerPredicate(Instance instance) {
        return player -> player.getInstance() == instance;
    }
}
