package me.hsgamer.flexgames.board;

import me.hsgamer.hscore.common.CollectionUtils;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerDisconnectEvent;
import net.minestom.server.scoreboard.Sidebar;

import java.util.*;
import java.util.function.Function;

public class Board {
    private static final Map<Player, Sidebar> boards = new HashMap<>();
    private static final int MAX_LINES = 15;
    private static final String MAGIC_STRING = "boardLine_";
    private final List<Player> players = new ArrayList<>();
    private final Function<Player, Component> title;
    private final Function<Player, List<Component>> lines;

    public Board(Function<Player, Component> title, Function<Player, List<Component>> lines) {
        this.title = title;
        this.lines = lines;
    }

    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerDisconnectEvent.class, event -> Board.removeBoard(event.getPlayer()));
    }

    private static void initBoard(Player player) {
        if (boards.containsKey(player)) {
            return;
        }
        Sidebar sidebar = new Sidebar(Component.text(""));
        sidebar.addViewer(player);
        boards.put(player, sidebar);
    }

    private static void removeBoard(Player player) {
        Optional.ofNullable(boards.remove(player)).ifPresent(sidebar -> sidebar.removeViewer(player));
    }

    public void addPlayer(Player player) {
        initBoard(player);
        players.add(player);
        update(player);
    }

    public void removePlayer(Player player) {
        players.remove(player);
    }

    public void removeAll() {
        players.clear();
    }

    public boolean update(Player player) {
        Sidebar sidebar = boards.get(player);
        if (sidebar == null) {
            return false;
        }

        updateBoard(player, sidebar);
        return true;
    }

    public void updateAll() {
        for (Player player : players) {
            update(player);
        }
    }

    private void updateBoard(Player player, Sidebar sidebar) {
        sidebar.setTitle(title.apply(player));

        List<Component> lineList = CollectionUtils.reverse(lines.apply(player));
        for (int i = 0; i < MAX_LINES; i++) {
            String lineId = MAGIC_STRING + i;
            Sidebar.ScoreboardLine line = sidebar.getLine(lineId);
            Component component = i < lineList.size() ? lineList.get(i) : null;
            if (line == null && component == null) {
                break;
            } else if (line == null) {
                sidebar.createLine(new Sidebar.ScoreboardLine(lineId, component, i));
            } else if (component == null) {
                sidebar.removeLine(lineId);
            } else {
                sidebar.updateLineContent(lineId, component);
            }
        }
    }
}
