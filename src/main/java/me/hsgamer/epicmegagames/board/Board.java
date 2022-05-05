package me.hsgamer.epicmegagames.board;

import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.scoreboard.Sidebar;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class Board {
    private static final int MAX_LINES = 15;

    private final Map<Player, Sidebar> boards = new HashMap<>();

    private final Function<Player, Component> title;
    private final Function<Player, List<Component>> lines;

    public Board(Function<Player, Component> title, Function<Player, List<Component>> lines) {
        this.title = title;
        this.lines = lines;
    }

    public boolean addPlayer(Player player) {
        if (boards.containsKey(player)) {
            return false;
        }

        Sidebar sidebar = new Sidebar(title.apply(player));
        List<Component> lineList = lines.apply(player).stream().limit(MAX_LINES).toList();

        for (int index = 0; index < lineList.size(); index++) {
            Component component = lineList.get(index);
            sidebar.createLine(new Sidebar.ScoreboardLine("line" + index, component, lineList.size() - index));
        }

        sidebar.addViewer(player);
        boards.put(player, sidebar);
        return true;
    }

    public boolean removePlayer(Player player) {
        Sidebar sidebar = boards.get(player);

        if (sidebar == null) {
            return false;
        }

        sidebar.removeViewer(player);
        boards.remove(player);
        return true;
    }

    public void removeAll() {
        for (Player player : boards.keySet()) {
            removePlayer(player);
        }
    }

    public boolean update(Player player) {
        Sidebar sidebar = boards.get(player);

        if (sidebar == null) {
            return false;
        }

        sidebar.setTitle(title.apply(player));
        List<Component> linesList = lines.apply(player);

        for (Sidebar.ScoreboardLine line : sidebar.getLines()) {
            int number = Integer.parseInt(line.getId().split("line")[1]);
            sidebar.updateLineContent(line.getId(), linesList.get(number));
        }

        return true;
    }

    public void updateAll() {
        for (Player player : boards.keySet()) {
            update(player);
        }
    }
}
