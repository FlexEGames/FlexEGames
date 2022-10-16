package me.hsgamer.flexegames.template.duel.feature;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.ExplosionEvent;
import io.github.bloepiloepi.pvp.events.FinalDamageEvent;
import io.github.bloepiloepi.pvp.events.PlayerExhaustEvent;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.template.duel.DuelGameConfig;
import me.hsgamer.flexegames.template.duel.state.InGameState;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.flexegames.util.FullBrightDimension;
import me.hsgamer.flexegames.util.PvpUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
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

import java.util.List;

public class ArenaInstanceFeature implements Feature {
    private final Arena arena;
    private final InstanceContainer instance;
    private final EventNode<EntityEvent> entityEventNode;
    private final Tag<Boolean> deadTag = Tag.Boolean("duel:dead").defaultValue(false);
    private final Tag<Boolean> playerBlockTag = Tag.Boolean("duel:playerBlock").defaultValue(false);

    public ArenaInstanceFeature(Arena arena) {
        this.arena = arena;
        this.instance = MinecraftServer.getInstanceManager().createInstanceContainer(FullBrightDimension.INSTANCE);
        entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
    }

    private boolean isFinished() {
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
        entityEventNode.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(gameConfig.getJoinPos()));
        MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
        PvpUtil.applyPvp(instance.eventNode(), gameConfig.isUseLegacyPvp());
        PvpUtil.applyExplosion(instance);
        instance.eventNode()
                .addListener(EntityPreDeathEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        event.setCancelled(true);
                        onKill(player);
                    }
                })
                .addListener(PlayerExhaustEvent.class, event -> {
                    if (isFinished() || Boolean.TRUE.equals(event.getEntity().getTag(deadTag))) {
                        event.setCancelled(true);
                    }
                })
                .addListener(FinalDamageEvent.class, event -> {
                    if (isFinished() || Boolean.TRUE.equals(event.getEntity().getTag(deadTag))) {
                        event.setCancelled(true);
                    }
                })
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.setRespawnPoint(gameConfig.getJoinPos());
                        player.setGameMode(GameMode.SURVIVAL);
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
    }

    @Override
    public void clear() {
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
        if (isFinished()) {
            player.setTag(deadTag, true);
            player.setGameMode(GameMode.SPECTATOR);
            player.setInvisible(true);
        }
    }
}
