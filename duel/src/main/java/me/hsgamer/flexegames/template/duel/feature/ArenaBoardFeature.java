package me.hsgamer.flexegames.template.duel.feature;

import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.template.duel.DuelGameConfig;
import me.hsgamer.flexegames.template.duel.state.EndingState;
import me.hsgamer.flexegames.template.duel.state.InGameState;
import me.hsgamer.flexegames.template.duel.state.WaitingState;
import me.hsgamer.hscore.minestom.board.Board;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.Collections;
import java.util.List;

public class ArenaBoardFeature implements Feature {
    private final Arena arena;
    private Board board;
    private Task task;

    public ArenaBoardFeature(Arena arena) {
        this.arena = arena;
    }

    @Override
    public void init() {
        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
        var instanceFeature = arena.getArenaFeature(InstanceFeature.class);
        var instance = instanceFeature.getInstance();
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class);
        this.board = new Board(
                player -> ReplacementManager.builder()
                        .replaceGlobal()
                        .replace(descriptionFeature.getReplacements())
                        .replacePlayer(player)
                        .build(gameConfig.getBoardTitle()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder()
                            .replaceGlobal()
                            .replace(descriptionFeature.getReplacements())
                            .replacePlayer(player);
                    List<Component> components = Collections.emptyList();
                    if (arena.getState() == WaitingState.class) {
                        components = gameConfig.getBoardLinesWaiting();
                    } else if (arena.getState() == InGameState.class) {
                        components = gameConfig.getBoardLinesIngame();
                    } else if (arena.getState() == EndingState.class) {
                        components = gameConfig.getBoardLinesEnding();
                    }
                    return components.stream().map(builder::build).toList();
                }
        );
        instance.eventNode()
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        board.addPlayer(player);
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        board.removePlayer(player);
                    }
                });
        task = instance.scheduler()
                .buildTask(board::updateAll)
                .repeat(TaskSchedule.nextTick())
                .executionType(ExecutionType.ASYNC)
                .schedule();
    }

    @Override
    public void clear() {
        task.cancel();
        board.removeAll();
    }
}
