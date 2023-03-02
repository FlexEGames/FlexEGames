package me.hsgamer.flexegames.game.duel.feature;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.FinalDamageEvent;
import io.github.bloepiloepi.pvp.events.PlayerExhaustEvent;
import lombok.Getter;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.feature.arena.KitFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.DuelProperties;
import me.hsgamer.flexegames.game.duel.state.InGameState;
import me.hsgamer.flexegames.game.duel.world.DuelWorld;
import me.hsgamer.flexegames.util.ChatUtil;
import me.hsgamer.flexegames.util.PlayerBlockUtil;
import me.hsgamer.flexegames.util.PvpUtil;
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

import java.util.List;

public class InstanceFeature implements Feature {
    private final Arena arena;
    private final DuelExtension duelExtension;
    private final Tag<Boolean> deadTag = Tag.Boolean("duel:dead").defaultValue(false);
    private @Getter DuelWorld duelWorld;
    private @Getter Instance instance;
    private EventNode<EntityEvent> entityEventNode;

    public InstanceFeature(Arena arena, DuelExtension duelExtension) {
        this.arena = arena;
        this.duelExtension = duelExtension;
    }

    private boolean isInGame() {
        return arena.getCurrentState() == InGameState.class;
    }

    @Override
    public void init() {
        var propertyMap = arena.getFeature(GameFeature.class).propertyMap();
        var descriptionFeature = arena.getFeature(DescriptionFeature.class);

        this.duelWorld = duelExtension.getDuelWorldManager().getDuelWorld(propertyMap.getProperty(DuelProperties.WORLD));

        this.instance = duelWorld.createInstance(arena);
        var instanceEventNode = instance.eventNode();
        entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
        entityEventNode.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(duelWorld.getJoinPos()));
        MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
        PvpUtil.applyPvp(instanceEventNode, propertyMap.getProperty(DuelProperties.LEGACY_PVP));
        PvpUtil.applyExplosion(instance);
        ChatUtil.apply(instanceEventNode, duelExtension.getMessageConfig().getChatFormat(), player -> descriptionFeature.getReplacements());
        PlayerBlockUtil.apply(instanceEventNode);
        instanceEventNode
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
                    }
                });
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

    public void sendMessage(Component component) {
        instance.sendMessage(component);
    }

    public void clearKit(Player player) {
        arena.getFeature(KitFeature.class).clearKit(player);
    }

    public void clearKit() {
        arena.getFeature(KitFeature.class).clearKit(instance);
    }

    public void giveKit() {
        arena.getFeature(KitFeature.class).giveKit(instance);
    }

    private void onKill(Player player) {
        player.heal();
        player.setFood(20);
        if (isInGame()) {
            clearKit(player);
            player.setTag(deadTag, true);
            player.setGameMode(GameMode.SPECTATOR);
            player.setInvisible(true);
        }
    }
}
