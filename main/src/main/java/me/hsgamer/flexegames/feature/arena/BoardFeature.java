package me.hsgamer.flexegames.feature.arena;

import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.hscore.minestom.board.Board;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.List;

/**
 * The feature to show the {@link Board} for all players in the {@link Instance}.
 * It would replace the placeholders in the title and lines, and update the board every tick.
 * The placeholders are took from the {@link DescriptionFeature} of the {@link Arena}.
 */
public abstract class BoardFeature implements Feature {
    /**
     * The arena
     */
    protected final Arena arena;
    private Board board;
    private Task task;
    private EventNode<InstanceEvent> boardEventNode;

    /**
     * Create a new feature
     *
     * @param arena the arena
     */
    protected BoardFeature(Arena arena) {
        this.arena = arena;
    }

    /**
     * Get the instance
     *
     * @return the instance
     */
    protected abstract Instance getInstance();

    /**
     * Get the title
     *
     * @param player the player
     * @return the title
     */
    protected abstract Component getTitle(Player player);

    /**
     * Get the lines
     *
     * @param player the player
     * @return the lines
     */
    protected abstract List<Component> getLines(Player player);

    @Override
    public void init() {
        var instance = getInstance();
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
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        this.board = new Board(
                player -> ReplacementManager.builder()
                        .replaceGlobal()
                        .replace(descriptionFeature.getReplacements())
                        .replacePlayer(player)
                        .build(getTitle(player)),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder()
                            .replaceGlobal()
                            .replace(descriptionFeature.getReplacements())
                            .replacePlayer(player);
                    List<Component> components = getLines(player);
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
