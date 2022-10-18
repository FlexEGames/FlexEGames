package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.network.packet.server.play.ScoreboardObjectivePacket;
import net.minestom.server.scoreboard.TabList;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@UtilityClass
public class PerInstanceTabListHook {
    private static final Map<Instance, TabList> TAB_LIST_MAP = new ConcurrentHashMap<>();

    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerSpawnEvent.class, event -> {
            var instance = event.getSpawnInstance();
            var player = event.getPlayer();
            var tabList = getTabList(instance);
            tabList.addViewer(player);
        }).addListener(RemoveEntityFromInstanceEvent.class, event -> {
            if (event.getEntity() instanceof Player player) {
                var instance = event.getInstance();
                var tabList = getTabList(instance);
                tabList.removeViewer(player);
            }
        });
    }

    public static TabList getTabList(Instance instance) {
        return TAB_LIST_MAP.computeIfAbsent(instance, i -> new TabList("tablist-" + i.getUniqueId(), ScoreboardObjectivePacket.Type.INTEGER));
    }
}
