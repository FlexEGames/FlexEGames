package me.hsgamer.flexegames.game.duel.feature;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.ExplosionEvent;
import io.github.bloepiloepi.pvp.events.FinalDamageEvent;
import io.github.bloepiloepi.pvp.events.PlayerExhaustEvent;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.game.duel.DuelGameConfig;
import me.hsgamer.flexegames.game.duel.state.EndingState;
import me.hsgamer.flexegames.game.duel.state.InGameState;
import me.hsgamer.flexegames.game.duel.state.WaitingState;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.flexegames.util.ChatUtil;
import me.hsgamer.flexegames.util.FullBrightDimension;
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
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.instance.block.Block;
import net.minestom.server.tag.Tag;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;

import java.util.Collections;
import java.util.List;

public class ArenaInstanceFeature implements Feature {
    private final Arena arena;
    private final InstanceContainer instance;
    private final EventNode<EntityEvent> entityEventNode;
    private final Tag<Boolean> deadTag = Tag.Boolean("duel:dead").defaultValue(false);
    private final Tag<Boolean> playerBlockTag = Tag.Boolean("duel:playerBlock").defaultValue(false);
    private Board board;
    private Task task;

    public ArenaInstanceFeature(Arena arena) {
        this.arena = arena;
        this.instance = MinecraftServer.getInstanceManager().createInstanceContainer(FullBrightDimension.INSTANCE);
        entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
    }

    private boolean isInGame() {
        return arena.getState() == InGameState.class;
    }

    @Override
    public void init() {
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class);
        instance.setTimeRate(0);
        instance.setTime(6000);
        instance.getWorldBorder().setCenter((float) gameConfig.getJoinPos().x(), (float) gameConfig.getJoinPos().z());
        instance.getWorldBorder().setDiameter(gameConfig.getBorderDiameter());

        boolean setGenerator = true;
        if (gameConfig.isUseWorld()) {
            IChunkLoader chunkLoader = gameConfig.getWorldLoader().getLoader(instance, AssetUtil.getWorldFile(gameConfig.getWorldName()).toPath());
            if (chunkLoader != null) {
                instance.setChunkLoader(chunkLoader);
                setGenerator = false;
            }
        }
        if (setGenerator) {
            instance.setGenerator(unit -> {
                unit.modifier().fillHeight(0, 1, Block.BEDROCK);
                if (gameConfig.getMaxHeight() > 1) {
                    unit.modifier().fillHeight(1, gameConfig.getMaxHeight(), Block.GRASS_BLOCK);
                }
            });
        }

        var descriptionFeature = arena.getArenaFeature(DescriptionFeature.class);
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

        entityEventNode.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(gameConfig.getJoinPos()));
        MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
        PvpUtil.applyPvp(instance.eventNode(), gameConfig.isUseLegacyPvp());
        PvpUtil.applyExplosion(instance);
        ChatUtil.apply(instance.eventNode(), gameConfig.getChatFormat(), player -> arena.getArenaFeature(DescriptionFeature.class).getReplacements());
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
                        player.setRespawnPoint(gameConfig.getJoinPos());
                        player.setGameMode(GameMode.SURVIVAL);
                        board.addPlayer(player);
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    if (!instance.isInVoid(event.getNewPosition())) return;
                    event.setNewPosition(gameConfig.getJoinPos());
                    if (arena.getState() == InGameState.class) {
                        onKill(event.getPlayer());
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.removeTag(deadTag);
                        board.removePlayer(player);
                    }
                })
                .addListener(PlayerBlockBreakEvent.class, event -> {
                    if (Boolean.FALSE.equals(event.getBlock().getTag(playerBlockTag))) {
                        event.setCancelled(true);
                    }
                })
                .addListener(ExplosionEvent.class, event -> event.getAffectedBlocks().removeIf(point -> !Boolean.TRUE.equals(instance.getBlock(point).getTag(playerBlockTag))))
                .addListener(PlayerBlockPlaceEvent.class, event -> event.setBlock(event.getBlock().withTag(playerBlockTag, true)));

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

    public InstanceContainer getInstance() {
        return instance;
    }

    private void onKill(Player player) {
        player.heal();
        player.setFood(20);
        player.getInventory().clear();
        if (isInGame()) {
            player.setTag(deadTag, true);
            player.setGameMode(GameMode.SPECTATOR);
            player.setInvisible(true);
        }
    }
}
