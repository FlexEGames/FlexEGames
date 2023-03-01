package me.hsgamer.flexegames.game.pve.feature;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.PlayerExhaustEvent;
import lombok.Getter;
import me.hsgamer.flexegames.api.property.PropertyMap;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.GameFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.PveGame;
import me.hsgamer.flexegames.game.pve.PveProperties;
import me.hsgamer.flexegames.game.pve.instance.ArenaInstance;
import me.hsgamer.flexegames.game.pve.state.EndingState;
import me.hsgamer.flexegames.game.pve.state.FightingState;
import me.hsgamer.flexegames.game.pve.state.RestingState;
import me.hsgamer.flexegames.game.pve.state.WaitingState;
import me.hsgamer.flexegames.helper.kit.GameKit;
import me.hsgamer.flexegames.util.ChatUtil;
import me.hsgamer.flexegames.util.PlayerBlockUtil;
import me.hsgamer.flexegames.util.PvpUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
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
    private static final Pos SPAWN_POS = new Pos(0, PveGame.HEIGHT, 0);
    private static final Tag<Boolean> DEAD_TAG = Tag.Boolean("pve:dead").defaultValue(false);
    private final Arena arena;
    private final PveExtension pveExtension;
    private final Instance instance;
    private final EventNode<EntityEvent> entityEventNode;
    private final @Getter GameKit gameKit;

    public InstanceFeature(Arena arena, PveExtension pveExtension, PropertyMap propertyMap) {
        this.arena = arena;
        this.pveExtension = pveExtension;
        this.instance = new ArenaInstance();
        entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
        this.gameKit = pveExtension.getGameKitManager().getGameKit(propertyMap.getProperty(PveProperties.KIT));
    }

    public Instance getInstance() {
        return instance;
    }

    @Override
    public void init() {
        var propertyMap = arena.getFeature(GameFeature.class).propertyMap();

        entityEventNode.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(SPAWN_POS));
        PvpUtil.applyExplosion(instance);
        PvpUtil.applyPvp(instance.eventNode(), propertyMap.getProperty(PveProperties.LEGACY_PVP));
        ChatUtil.apply(instance.eventNode(), pveExtension.getMessageConfig().getChatFormat(), player -> arena.getFeature(DescriptionFeature.class).getReplacements());
        PlayerBlockUtil.apply(instance.eventNode());

        instance.eventNode()
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.setRespawnPoint(SPAWN_POS);
                        player.setGameMode(GameMode.SURVIVAL);
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        player.removeTag(DEAD_TAG);
                    }
                })
                .addListener(EntityPreDeathEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        event.setCancelled(true);
                        onKill(player);
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    if (!instance.isInVoid(event.getNewPosition())) return;
                    event.setNewPosition(SPAWN_POS);
                    if (isInGame()) {
                        onKill(event.getPlayer());
                    }
                })
                .addListener(PlayerExhaustEvent.class, event -> {
                    if (arena.getCurrentState() == WaitingState.class || arena.getCurrentState() == EndingState.class) {
                        event.setCancelled(true);
                    }
                });

        MinecraftServer.getInstanceManager().registerInstance(instance);
        MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
    }

    @Override
    public void clear() {
        arena.getFeature(LobbyFeature.class).send(instance.getPlayers());
        MinecraftServer.getInstanceManager().unregisterInstance(instance);
        MinecraftServer.getGlobalEventHandler().removeChild(entityEventNode);
    }

    public void sendMessage(Component component) {
        instance.sendMessage(component);
    }

    public void giveKit(Player player) {
        gameKit.giveItems(player);
    }

    public void giveKit() {
        instance.getPlayers().forEach(this::giveKit);
    }

    public void clearInventory(Player player) {
        player.getInventory().clear();
    }

    public void clearInventory() {
        instance.getPlayers().forEach(this::clearInventory);
    }

    public void respawnDeadPlayers() {
        instance.getPlayers().stream()
                .filter(player -> Boolean.TRUE.equals(player.getTag(DEAD_TAG)))
                .forEach(player -> {
                    player.setTag(DEAD_TAG, false);
                    player.heal();
                    player.setGameMode(GameMode.SURVIVAL);
                    giveKit(player);
                    player.teleport(SPAWN_POS);
                });
    }

    public void tryHealAll() {
        if (arena.getFeature(GameFeature.class).propertyMap().getProperty(PveProperties.HEAL_ON_REST)) {
            instance.getPlayers().forEach(player -> {
                player.heal();
                player.setFood(20);
            });
        }
    }

    public boolean isAllDead() {
        return instance.getPlayers().stream().allMatch(player -> Boolean.TRUE.equals(player.getTag(DEAD_TAG)));
    }

    public List<Player> getAlivePlayers() {
        return instance.getPlayers().stream().filter(player -> Boolean.FALSE.equals(player.getTag(DEAD_TAG))).toList();
    }

    private boolean isInGame() {
        return arena.getCurrentState() == FightingState.class || arena.getCurrentState() == RestingState.class;
    }

    private boolean isFighting() {
        return arena.getCurrentState() == FightingState.class;
    }

    private void onKill(Player player) {
        player.heal();
        player.setFood(20);
        if (isFighting()) {
            clearInventory(player);
            player.setTag(DEAD_TAG, true);
            player.setGameMode(GameMode.SPECTATOR);
            player.setInvisible(true);
        }
    }
}
