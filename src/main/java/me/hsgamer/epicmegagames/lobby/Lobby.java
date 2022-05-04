package me.hsgamer.epicmegagames.lobby;

import com.sqcred.sboards.SBoard;
import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.util.FullBrightDimension;
import me.hsgamer.epicmegagames.util.LoaderType;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.item.ItemDropEvent;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;
import org.jglrxavpok.hephaistos.nbt.NBTCompound;

import java.util.UUID;

public class Lobby extends InstanceContainer {
    private final Pos position;
    private final SBoard board;
    private final Task boardTask;

    public Lobby() {
        super(UUID.randomUUID(), FullBrightDimension.INSTANCE);
        position = LobbyConfig.POSITION.getValue();
        board = new SBoard(
                player -> replaceText(player, LobbyConfig.BOARD_TITLE.getValue()),
                player -> LobbyConfig.BOARD_LINES.getValue().stream()
                        .map(line -> replaceText(player, line))
                        .toList()
        );
        setTimeRate(0);

        LoaderType worldType = LobbyConfig.WORLD_TYPE.getValue();
        setChunkLoader(worldType.getLoader(this, LobbyConfig.WORLD_NAME.getValue()));

        EventNode<InstanceEvent> eventNode = eventNode();
        eventNode
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    final Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        final Instance instance = player.getInstance();
                        if (instance != null)
                            player.scheduler().scheduleNextTick(() -> onArenaFinish(player));
                        else
                            onFirstSpawn(player);
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    final Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        board.removePlayer(player);
                    }
                })
                .addListener(ItemDropEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockBreakEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockPlaceEvent.class, event -> event.setCancelled(true));
        boardTask = MinecraftServer.getSchedulerManager().buildTask(board::updateAll)
                .repeat(TaskSchedule.tick(LobbyConfig.BOARD_UPDATE_TIME.getValue()))
                .schedule();
    }

    private Component replaceText(Player player, Component component) {
        return component
                .replaceText(builder -> builder.match("%%player").replacement(player.getName()));
    }

    void onFirstSpawn(Player player) {
        board.addPlayer(player);
    }

    void onArenaFinish(Player player) {
        board.addPlayer(player);
        player.refreshCommands();
        player.getInventory().clear();
        player.teleport(position);
        player.tagHandler().updateContent(NBTCompound.EMPTY);
    }

    public Pos getPosition() {
        return position;
    }

    public void addPlayer(Player player) {
        player.setInstance(this);
    }

    public void clear() {
        boardTask.cancel();
    }
}
