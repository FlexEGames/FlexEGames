package me.hsgamer.epicmegagames.lobby;

import me.hsgamer.epicmegagames.api.ChunkLoaderType;
import me.hsgamer.epicmegagames.board.Board;
import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import me.hsgamer.epicmegagames.util.FullBrightDimension;
import me.hsgamer.epicmegagames.util.TaskUtil;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.inventory.InventoryPreClickEvent;
import net.minestom.server.event.item.ItemDropEvent;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.player.PlayerSwapItemEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.network.packet.server.play.TeamsPacket;
import net.minestom.server.scoreboard.Team;
import net.minestom.server.timer.Task;

import java.util.UUID;

public class Lobby extends InstanceContainer {
    private final Pos position;
    private final Board board;
    private final Task boardTask;
    private final Team lobbyTeam;

    public Lobby() {
        super(UUID.randomUUID(), FullBrightDimension.INSTANCE);
        position = LobbyConfig.POSITION.getValue();
        board = new Board(
                player -> ReplacementManager.builder().replaceGlobal().replacePlayer(player).build(LobbyConfig.BOARD_TITLE.getValue()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder().replaceGlobal().replacePlayer(player);
                    return LobbyConfig.BOARD_LINES.getValue().stream()
                            .map(builder::build)
                            .toList();
                }
        );
        setTimeRate(0);

        ChunkLoaderType worldType = LobbyConfig.WORLD_TYPE.getValue();
        setChunkLoader(worldType.getLoader(this, LobbyConfig.WORLD_NAME.getValue()));

        EventNode<InstanceEvent> eventNode = eventNode();
        eventNode
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        Instance instance = player.getInstance();
                        if (instance != null)
                            player.scheduler().scheduleNextTick(() -> onBackSpawn(player));
                        else
                            onFirstSpawn(player);
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        onQuit(player);
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    if (isInVoid(event.getNewPosition())) {
                        event.setNewPosition(position);
                    }
                })
                .addListener(ItemDropEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockBreakEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockPlaceEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerSwapItemEvent.class, event -> event.setCancelled(true))
                .addListener(InventoryPreClickEvent.class, event -> {
                    if (event.getInventory() == null) {
                        event.setCancelled(true);
                    }
                });
        boardTask = MinecraftServer.getSchedulerManager().buildTask(board::updateAll)
                .repeat(TaskUtil.tick(LobbyConfig.BOARD_UPDATE_TIME.getValue()))
                .schedule();
        lobbyTeam = MinecraftServer.getTeamManager().createBuilder("lobbyTeam")
                .collisionRule(TeamsPacket.CollisionRule.NEVER)
                .build();
    }

    void onFirstSpawn(Player player) {
        board.addPlayer(player);
        player.setRespawnPoint(position);
        player.setEnableRespawnScreen(false);
        player.setGameMode(GameMode.ADVENTURE);
        player.setTeam(lobbyTeam);
    }

    void onBackSpawn(Player player) {
        board.addPlayer(player);
        player.setGameMode(GameMode.ADVENTURE);
        player.heal();
        player.setFood(20);
        player.setLevel(0);
        player.setAllowFlying(false);
        player.setFlying(false);
        player.setInvisible(false);
        player.setAdditionalHearts(0);
        player.clearEffects();
        player.getInventory().clear();
        player.setNoGravity(false);
        player.setExp(0);
        player.stopSpectating();
        player.askSynchronization();
        player.teleport(position);
        player.setRespawnPoint(position);
        player.setEnableRespawnScreen(false);
        player.setTeam(lobbyTeam);
    }

    void onQuit(Player player) {
        board.removePlayer(player);
        player.setTeam(null);
    }

    public Pos getPosition() {
        return position;
    }

    public void clear() {
        boardTask.cancel();
    }

    public boolean isInLobby(Player player) {
        return player.getInstance() == null || player.getInstance() == this;
    }
}
