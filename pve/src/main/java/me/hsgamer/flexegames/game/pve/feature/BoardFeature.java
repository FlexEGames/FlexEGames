package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.state.EndingState;
import me.hsgamer.flexegames.game.pve.state.FightingState;
import me.hsgamer.flexegames.game.pve.state.RestingState;
import me.hsgamer.flexegames.game.pve.state.WaitingState;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.hscore.minestom.board.Board;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.Collections;
import java.util.List;

public class BoardFeature extends ArenaFeature<BoardFeature.ArenaBoardFeature> {
    @Override
    protected ArenaBoardFeature createFeature(Arena arena) {
        return new ArenaBoardFeature(arena);
    }

    public static class ArenaBoardFeature implements Feature {
        private final Arena arena;
        private Board board;
        private Task task;
        private EventNode<InstanceEvent> boardEventNode;

        public ArenaBoardFeature(Arena arena) {
            this.arena = arena;
        }

        @Override
        public void init() {
            var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);
            var instance = arena.getArenaFeature(InstanceFeature.class).getInstance();
            var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
            boardEventNode = EventNode.event("boardEventNode-" + arena.getName(), EventFilter.INSTANCE, event -> event.getInstance() == instance);
            MinecraftServer.getGlobalEventHandler().addChild(boardEventNode);
            boardEventNode
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
                        var state = arena.getState();
                        List<Component> components = Collections.emptyList();
                        if (state == WaitingState.class) {
                            components = gameConfig.getBoardLinesWaiting();
                        } else if (state == RestingState.class) {
                            components = gameConfig.getBoardLinesResting();
                        } else if (state == FightingState.class) {
                            components = gameConfig.getBoardLinesFighting();
                        } else if (state == EndingState.class) {
                            components = gameConfig.getBoardLinesEnding();
                        }
                        return components.stream().map(builder::build).toList();
                    }
            );
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
            MinecraftServer.getGlobalEventHandler().removeChild(boardEventNode);
        }
    }
}
