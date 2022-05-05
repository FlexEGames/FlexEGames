package me.hsgamer.epicmegagames.template.duel;

import me.hsgamer.epicmegagames.api.ArenaGame;
import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.config.path.PosListPath;
import me.hsgamer.epicmegagames.config.path.PosPath;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.coordinate.Pos;

import java.util.List;

public class DuelTemplate implements Template {
    private static final ConfigPath<List<Pos>> posPath = new PosListPath("pos", List.of(
            new Pos(-2, 4, 0),
            new Pos(2, 4, 0),
            new Pos(0, 4, -2),
            new Pos(0, 4, 2)
    ));
    private static final ConfigPath<Pos> joinPosPath = new PosPath("join-pos", new Pos(0, 3, 0));
    private static final ConfigPath<Integer> maxHeightPath = Paths.integerPath("max-height", 2);
    private static final ConfigPath<Integer> waitingTimePath = Paths.integerPath("waiting-time", 60);
    private static final ConfigPath<Integer> endingTimePath = Paths.integerPath("ending-time", 5);
    final List<Pos> posList;
    final Pos joinPos;
    final int maxHeight;
    final int waitingTime;
    final int endingTime;

    public DuelTemplate(Config config) {
        posList = posPath.getValue(config);
        joinPos = joinPosPath.getValue(config);
        maxHeight = maxHeightPath.getValue(config);
        waitingTime = waitingTimePath.getValue(config);
        endingTime = endingTimePath.getValue(config);
    }

    @Override
    public ArenaGame createGame(Arena arena) {
        return new DuelGame(this, arena);
    }
}
