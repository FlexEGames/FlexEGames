package me.hsgamer.flexegames.template.duel;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.FinalDamageEvent;
import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.api.ArenaGame;
import me.hsgamer.flexegames.api.JoinResponse;
import me.hsgamer.flexegames.api.Template;
import me.hsgamer.flexegames.board.Board;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.state.EndingState;
import me.hsgamer.flexegames.state.InGameState;
import me.hsgamer.flexegames.state.WaitingState;
import me.hsgamer.flexegames.util.FullBrightDimension;
import me.hsgamer.flexegames.util.PvpUtil;
import me.hsgamer.flexegames.util.TemplateUtil;
import me.hsgamer.flexegames.util.TimeUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import me.hsgamer.minigamecore.implementation.feature.single.TimerFeature;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.instance.block.Block;
import net.minestom.server.item.ItemStack;
import net.minestom.server.tag.Tag;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

@ExtensionMethod({PvpUtil.class})
public class DuelGame implements ArenaGame {
    private final DuelTemplate template;
    private final Arena arena;
    private final TimerFeature timerFeature;
    private final InstanceContainer instance;
    private final AtomicBoolean isFinished = new AtomicBoolean(false);
    private final Tag<Boolean> deadTag = Tag.Boolean("dead").defaultValue(false);
    private final Tag<Boolean> playerBlockTag = Tag.Boolean("playerBlock").defaultValue(false);
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
                        .replace(getReplacements())
                        .replacePlayer(player)
                        .build(MessageConfig.GAME_DUEL_BOARD_TITLE.getValue()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder()
                            .replaceGlobal()
                            .replace(getReplacements())
                            .replacePlayer(player);
                    List<Component> components = Collections.emptyList();
                    if (arena.getState() == WaitingState.class) {
                        components = MessageConfig.GAME_DUEL_BOARD_LINES_WAITING.getValue();
                    } else if (arena.getState() == InGameState.class) {
                        components = MessageConfig.GAME_DUEL_BOARD_LINES_INGAME.getValue();
                    } else if (arena.getState() == EndingState.class) {
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
    public Template getTemplate() {
        return template;
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemBuilder.buildItem(template.gameDisplayItem, getReplacements());
    }

    private Map<String, Supplier<ComponentLike>> getReplacements() {
        return Map.of(
                "players", () -> Component.text(Integer.toString(instance.getPlayers().size())),
                "time", () -> Component.text(TimeUtil.format(timerFeature.getDuration(TimeUnit.MILLISECONDS))),
                "max-players", () -> Component.text(Integer.toString(template.posList.size())),
                "state", () -> arena.getStateInstance().map(GameState::getDisplayName).map(LegacyComponentSerializer.legacyAmpersand()::deserialize).orElse(Component.empty()),
                "template", () -> template.displayName,
                "owner", () -> TemplateUtil.getOwner(arena),
                "name", () -> Component.text(arena.getName()),
                "winner", () -> Optional.ofNullable(winner.get()).map(Player::getName).orElse(Component.empty()),
                "alive", () -> Component.text(Integer.toString(getAlivePlayers().size()))
        );
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
        instance.setGenerator(unit -> {
            unit.modifier().fillHeight(0, 1, Block.BEDROCK);
            if (template.maxHeight > 1) {
                unit.modifier().fillHeight(1, template.maxHeight, Block.GRASS_BLOCK);
            }
        });
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
                        player.getInventory().clear();
                        player.removeTag(deadTag);
                        board.removePlayer(player);
                    }
                })
                .addListener(PlayerBlockBreakEvent.class, event -> {
                    if (Boolean.FALSE.equals(event.getBlock().getTag(playerBlockTag))) {
                        event.setCancelled(true);
                    }
                })
                .addListener(PlayerBlockPlaceEvent.class, event -> event.setBlock(event.getBlock().withTag(playerBlockTag, true)));
        task = instance.scheduler()
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
        return instance.getPlayers().stream().filter(player -> Boolean.FALSE.equals(player.tagHandler().getTag(deadTag))).toList();
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
            message = ReplacementManager.replace(MessageConfig.GAME_DUEL_WINNER_MESSAGE.getValue(), Map.of("player", winnerPlayer::getName));
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
