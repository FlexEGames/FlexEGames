package me.hsgamer.flexegames.game.pve.feature;

import io.github.bloepiloepi.pvp.events.EntityPreDeathEvent;
import io.github.bloepiloepi.pvp.events.PlayerExhaustEvent;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.game.pve.PveGame;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.instance.ArenaInstance;
import me.hsgamer.flexegames.game.pve.state.EndingState;
import me.hsgamer.flexegames.game.pve.state.FightingState;
import me.hsgamer.flexegames.game.pve.state.RestingState;
import me.hsgamer.flexegames.game.pve.state.WaitingState;
import me.hsgamer.flexegames.util.ChatUtil;
import me.hsgamer.flexegames.util.PlayerBlockUtil;
import me.hsgamer.flexegames.util.PvpUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
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

public class InstanceFeature extends ArenaFeature<InstanceFeature.ArenaInstanceFeature> {
    private static final Pos SPAWN_POS = new Pos(0, PveGame.HEIGHT, 0);
    private static final Tag<Boolean> DEAD_TAG = Tag.Boolean("pve:dead").defaultValue(false);

    @Override
    protected ArenaInstanceFeature createFeature(Arena arena) {
        return new ArenaInstanceFeature(arena);
    }

    public static class ArenaInstanceFeature implements Feature {
        private final Arena arena;
        private final Instance instance;
        private final EventNode<EntityEvent> entityEventNode;

        public ArenaInstanceFeature(Arena arena) {
            this.arena = arena;
            this.instance = new ArenaInstance();
            entityEventNode = EventNode.event("entityEvent-" + arena.getName(), EventFilter.ENTITY, entityEvent -> entityEvent.getEntity().getInstance() == instance);
        }

        public Instance getInstance() {
            return instance;
        }

        @Override
        public void init() {
            var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);

            entityEventNode.addListener(PlayerSpawnEvent.class, event -> event.getPlayer().teleport(SPAWN_POS));
            PvpUtil.applyExplosion(instance);
            PvpUtil.applyPvp(instance.eventNode(), gameConfig.isUseLegacyPvp());
            ChatUtil.apply(instance.eventNode(), gameConfig.getChatFormat(), player -> arena.getArenaFeature(DescriptionFeature.class).getReplacements());
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
                        if (arena.getState() == WaitingState.class || arena.getState() == EndingState.class) {
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
            var kit = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getConvertedKit();
            var inventory = player.getInventory();
            kit.forEach((slot, item) -> {
                if (slot < 0 || slot >= inventory.getSize()) return;
                player.getInventory().setItemStack(slot, item);
            });
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
            if (arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).isHealOnRest()) {
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
            return arena.getState() == FightingState.class || arena.getState() == RestingState.class;
        }

        private boolean isFighting() {
            return arena.getState() == FightingState.class;
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
}
