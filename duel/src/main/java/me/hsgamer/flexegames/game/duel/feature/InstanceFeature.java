package me.hsgamer.flexegames.game.duel.feature;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.FinalDamageEvent;
import io.github.bloepiloepi.pvp.events.PlayerExhaustEvent;
import lombok.Getter;
import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.DuelProperties;
import me.hsgamer.flexegames.game.duel.state.EndingState;
import me.hsgamer.flexegames.game.duel.state.InGameState;
import me.hsgamer.flexegames.game.duel.state.WaitingState;
import me.hsgamer.flexegames.game.duel.world.DuelWorld;
import me.hsgamer.flexegames.helper.kit.GameKit;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.ChatUtil;
import me.hsgamer.flexegames.util.PlayerBlockUtil;
import me.hsgamer.flexegames.util.PvpUtil;
import me.hsgamer.hscore.minestom.board.Board;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.tag.Tag;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.Collections;
import java.util.List;

public class InstanceFeature implements Feature {
    private final Arena arena;
    private final @Getter DuelWorld duelWorld;
    private final @Getter GameKit gameKit;
    private final DuelExtension duelExtension;
    private final @Getter Instance instance;
    private final EventNode<EntityEvent> entityEventNode;
    private final Tag<Boolean> deadTag = Tag.Boolean("duel:dead").defaultValue(false);
    private Board board;
    private Task task;

    public InstanceFeature(Arena arena, DuelExtension duelExtension, GamePropertyMap propertyMap) {
        this.arena = arena;
        this.duelWorld = duelExtension.getDuelWorldManager().getDuelWorld(propertyMap.getProperty(DuelProperties.WORLD));
        this.gameKit = duelExtension.getGameKitManager().getGameKit(propertyMap.getProperty(DuelProperties.KIT));
        this.duelExtension = duelExtension;
        this.instance = duelWorld.createInstance(arena);
        entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
    }

    private boolean isInGame() {
        return arena.getCurrentState() == InGameState.class;
    }

    @Override
    public void init() {
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);
        this.board = new Board(
                player -> ReplacementManager.builder()
                        .replaceGlobal()
                        .replace(descriptionFeature.getReplacements())
                        .replacePlayer(player)
                        .build(duelExtension.getMessageConfig().getBoardTitle()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder()
                            .replaceGlobal()
                            .replace(descriptionFeature.getReplacements())
                            .replacePlayer(player);
                    List<Component> components = Collections.emptyList();
                    if (arena.getCurrentState() == WaitingState.class) {
                        components = duelExtension.getMessageConfig().getBoardLinesWaiting();
                    } else if (arena.getCurrentState() == InGameState.class) {
                        components = duelExtension.getMessageConfig().getBoardLinesIngame();
                    } else if (arena.getCurrentState() == EndingState.class) {
                        components = duelExtension.getMessageConfig().getBoardLinesEnding();
                    }
                    return components.stream().map(builder::build).toList();
                }
        );

        entityEventNode.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(duelWorld.getJoinPos()));
        MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
        PvpUtil.applyPvp(instance.eventNode(), arena.getFeature(GameFeature.class).propertyMap().getProperty(DuelProperties.LEGACY_PVP));
        PvpUtil.applyExplosion(instance);
        ChatUtil.apply(instance.eventNode(), duelExtension.getMessageConfig().getChatFormat(), player -> descriptionFeature.getReplacements());
        PlayerBlockUtil.apply(instance.eventNode());
        instance.eventNode()
                .addListener(EntityPreDeathEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        event.setCancelled(true);
                        onKill(player);
                    }
                })
                .addListener(PlayerExhaustEvent.class, event -> {
                    if (!isInGame() || Boolean.TRUE.equals(event.getEntity().getTag(deadTag))) {
                        event.setCancelled(true);
                    }
                })
                .addListener(FinalDamageEvent.class, event -> {
                    if (!isInGame() || Boolean.TRUE.equals(event.getEntity().getTag(deadTag))) {
                        event.setCancelled(true);
                    }
                })
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.setRespawnPoint(duelWorld.getJoinPos());
                        player.setGameMode(GameMode.SURVIVAL);
                        board.addPlayer(player);
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    if (!instance.isInVoid(event.getNewPosition())) return;
                    event.setNewPosition(duelWorld.getJoinPos());
                    if (isInGame()) {
                        onKill(event.getPlayer());
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.removeTag(deadTag);
                        board.removePlayer(player);
                    }
                });

        MinecraftServer.getInstanceManager().registerInstance(instance);
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
        backToLobby();
        MinecraftServer.getGlobalEventHandler().removeChild(entityEventNode);
        MinecraftServer.getInstanceManager().unregisterInstance(instance);
    }

    public List<Player> getAlivePlayers() {
        return instance.getPlayers().stream().filter(player -> Boolean.FALSE.equals(player.tagHandler().getTag(deadTag))).toList();
    }

    public void backToLobby() {
        arena.getFeature(LobbyFeature.class).send(instance.getPlayers());
    }

    public void sendMessage(Component component) {
        instance.sendMessage(component);
    }

    public void clearInventory(Player player) {
        player.getInventory().clear();
    }

    public void clearInventory() {
        instance.getPlayers().forEach(this::clearInventory);
    }

    public void giveKit(Player player) {
        gameKit.giveItems(player);
    }

    public void giveKit() {
        instance.getPlayers().forEach(this::giveKit);
    }

    private void onKill(Player player) {
        player.heal();
        player.setFood(20);
        if (isInGame()) {
            clearInventory(player);
            player.setTag(deadTag, true);
            player.setGameMode(GameMode.SPECTATOR);
            player.setInvisible(true);
        }
    }
}
