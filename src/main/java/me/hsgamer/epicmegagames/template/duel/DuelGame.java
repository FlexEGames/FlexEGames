package me.hsgamer.epicmegagames.template.duel;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.FinalDamageEvent;
import lombok.experimental.ExtensionMethod;
import me.hsgamer.epicmegagames.api.ArenaGame;
import me.hsgamer.epicmegagames.api.JoinResponse;
import me.hsgamer.epicmegagames.board.Board;
import me.hsgamer.epicmegagames.config.MessageConfig;
import me.hsgamer.epicmegagames.feature.LobbyFeature;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import me.hsgamer.epicmegagames.state.EndingState;
import me.hsgamer.epicmegagames.state.InGameState;
import me.hsgamer.epicmegagames.state.WaitingState;
import me.hsgamer.epicmegagames.util.FullBrightDimension;
import me.hsgamer.epicmegagames.util.PvpUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import me.hsgamer.minigamecore.implementation.feature.single.TimerFeature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.instance.block.Block;
import net.minestom.server.tag.Tag;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

@ExtensionMethod({PvpUtil.class})
public class DuelGame implements ArenaGame {
    private final DuelTemplate template;
    private final Arena arena;
    private final TimerFeature timerFeature;
    private final InstanceContainer instance;
    private final AtomicBoolean isFinished = new AtomicBoolean(false);
    private final Tag<Boolean> deadTag = Tag.Boolean("dead");
    private final AtomicReference<Player> winner = new AtomicReference<>();
    private final Board board;
    private final EventNode<EntityEvent> entityEventNode;
    private Task task;

    public DuelGame(DuelTemplate template, Arena arena) {
        this.template = template;
        this.arena = arena;
        this.timerFeature = arena.getArenaFeature(ArenaTimerFeature.class);
        this.instance = MinecraftServer.getInstanceManager().createInstanceContainer(FullBrightDimension.INSTANCE);
        instance.getWorldBorder().setCenter((float) template.joinPos.x(), (float) template.joinPos.z());
        instance.getWorldBorder().setDiameter(template.borderDiameter);
        this.board = new Board(
                player -> ReplacementManager.builder()
                        .replaceGlobal()
                        .replacePlayer(player)
                        .build(MessageConfig.GAME_DUEL_BOARD_TITLE.getValue()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder()
                            .replaceGlobal()
                            .replacePlayer(player);
                    List<Component> components = Collections.emptyList();
                    if (arena.getState() == WaitingState.class) {
                        builder.replace(Map.of(
                                "time", Component.text(Long.toString(timerFeature.getDuration(TimeUnit.SECONDS))),
                                "players", Component.text(Integer.toString(instance.getPlayers().size()))
                        ));
                        components = MessageConfig.GAME_DUEL_BOARD_LINES_WAITING.getValue();
                    } else if (arena.getState() == InGameState.class) {
                        builder.replace(Map.of(
                                "players", Component.text(Integer.toString(getAlivePlayers().size()))
                        ));
                        components = MessageConfig.GAME_DUEL_BOARD_LINES_INGAME.getValue();
                    } else if (arena.getState() == EndingState.class) {
                        builder.replace(Map.of(
                                "time", Component.text(Long.toString(timerFeature.getDuration(TimeUnit.SECONDS))),
                                "winner", Optional.ofNullable(winner.get()).map(Player::getName).orElse(Component.empty())
                        ));
                        components = MessageConfig.GAME_DUEL_BOARD_LINES_ENDING.getValue();
                    }
                    return components.stream().map(builder::build).toList();
                }
        );
        entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
        entityEventNode
                .applyPvp(template.useLegacyPvp)
                .addListener(EntityPreDeathEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        event.setCancelled(true);
                        player.heal();
                        player.setFood(20);
                        player.getInventory().clear();
                        if (!isFinished.get()) {
                            player.setTag(deadTag, true);
                            player.setGameMode(GameMode.SPECTATOR);
                        }
                    }
                })
                .addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(template.joinPos))
                .addListener(FinalDamageEvent.class, event -> {
                    if (isFinished.get() || arena.getState() == WaitingState.class || arena.getState() == EndingState.class || Boolean.TRUE.equals(event.getEntity().getTag(deadTag))) {
                        event.setCancelled(true);
                    }
                });
    }

    @Override
    public JoinResponse join(Player player) {
        if (arena.getState() == WaitingState.class) {
            if (instance.getPlayers().size() >= template.posList.size()) {
                return JoinResponse.MAX_PLAYER_REACHED;
            }
            player.setInstance(instance);
            return JoinResponse.SUCCESSFUL_JOIN;
        }
        return JoinResponse.NOT_WAITING;
    }

    @Override
    public void init() {
        instance.setGenerator(unit -> unit.modifier().fillHeight(0, template.maxHeight, Block.GRASS_BLOCK));
        MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
        instance.eventNode()
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.setRespawnPoint(template.joinPos);
                        player.setGameMode(GameMode.SURVIVAL);
                        board.addPlayer(player);
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    if (instance.isInVoid(event.getNewPosition())) {
                        event.setNewPosition(template.joinPos);
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.removeTag(deadTag);
                        board.removePlayer(player);
                    }
                })
                .addListener(PlayerBlockBreakEvent.class, event -> event.setCancelled(true));
        task = MinecraftServer.getSchedulerManager()
                .buildTask(board::updateAll)
                .repeat(TaskSchedule.nextTick())
                .schedule();
    }

    @Override
    public void postInit() {
        MinecraftServer.getInstanceManager().registerInstance(instance);
    }

    @Override
    public void onWaitingStart() {
        timerFeature.setDuration(template.waitingTime, TimeUnit.SECONDS);
    }

    @Override
    public boolean isWaitingOver() {
        return timerFeature.getDuration(TimeUnit.MILLISECONDS) <= 0;
    }

    @Override
    public boolean canStart() {
        return instance.getPlayers().size() > 1;
    }

    @Override
    public void onFailedWaitingEnd() {
        Component component = MessageConfig.GAME_DUEL_NOT_ENOUGH_PLAYERS.getValue();
        for (Player player : instance.getPlayers()) {
            player.sendMessage(component);
        }
    }

    @Override
    public void onInGameStart() {
        List<Player> players = new ArrayList<>(instance.getPlayers());
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            giveKit(player);
            player.setTag(deadTag, false);
            Pos pos = template.posList.get(i % template.posList.size());
            player.teleport(pos);
        }
    }

    private void giveKit(Player player) {
        var inventory = player.getInventory();
        template.kit.forEach((slot, item) -> {
            if (slot < 0 || slot >= inventory.getSize()) return;
            player.getInventory().setItemStack(slot, item);
        });
    }

    private List<Player> getAlivePlayers() {
        return instance.getPlayers().stream().filter(player -> player.tagHandler().getTag(deadTag) != Boolean.TRUE).toList();
    }

    private void checkWinner() {
        List<Player> alivePlayers = getAlivePlayers();
        if (alivePlayers.size() <= 1) {
            isFinished.set(true);
            if (alivePlayers.size() == 1) {
                winner.set(alivePlayers.get(0));
            }
        }
    }

    @Override
    public boolean isInGameOver() {
        checkWinner();
        return isFinished.get();
    }

    @Override
    public void onEndingStart() {
        timerFeature.setDuration(template.endingTime, TimeUnit.SECONDS);
        Player winnerPlayer = winner.get();
        Component message;
        if (winnerPlayer != null) {
            message = ReplacementManager.replace(MessageConfig.GAME_DUEL_WINNER_MESSAGE.getValue(), Map.of("player", winnerPlayer.getName()));
        } else {
            message = MessageConfig.GAME_DUEL_NO_WINNER_MESSAGE.getValue();
        }
        for (Player player : instance.getPlayers()) {
            player.getInventory().clear();
            if (winnerPlayer != null) {
                player.sendMessage(message);
            }
        }
    }

    @Override
    public boolean isEndingOver() {
        return timerFeature.getDuration(TimeUnit.MILLISECONDS) <= 0;
    }

    @Override
    public void clear() {
        for (Player player : instance.getPlayers()) {
            arena.getFeature(LobbyFeature.class).backToLobby(player);
        }
        if (task != null) {
            task.cancel();
        }
        MinecraftServer.getGlobalEventHandler().removeChild(entityEventNode);
        MinecraftServer.getInstanceManager().unregisterInstance(instance);
    }
}
