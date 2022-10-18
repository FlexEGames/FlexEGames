package me.hsgamer.flexegames.game.pve.feature;

import io.github.bloepiloepi.pvp.config.ArmorToolConfig;
import io.github.bloepiloepi.pvp.config.ExplosionConfig;
import io.github.bloepiloepi.pvp.config.FoodConfig;
import io.github.bloepiloepi.pvp.config.PotionConfig;
import io.github.bloepiloepi.pvp.events.ExplosionEvent;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.LobbyFeature;
import me.hsgamer.flexegames.game.pve.PveGame;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.instance.ArenaInstance;
import me.hsgamer.flexegames.game.pve.instance.InstanceEventHook;
import me.hsgamer.flexegames.game.pve.state.FightingState;
import me.hsgamer.flexegames.game.pve.state.RestingState;
import me.hsgamer.flexegames.util.ChatUtil;
import me.hsgamer.flexegames.util.PvpUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.MinecraftServer;
import net.minestom.server.attribute.Attribute;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.*;
import net.minestom.server.entity.metadata.arrow.ArrowMeta;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.entity.EntityDamageEvent;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.player.PlayerSpawnEvent;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.tag.Tag;
import net.minestom.server.utils.time.TimeUnit;

import java.time.Duration;
import java.util.List;

public class InstanceFeature extends ArenaFeature<InstanceFeature.ArenaInstanceFeature> {
    private static final Pos SPAWN_POS = new Pos(0, PveGame.HEIGHT, 0);
    private static final Tag<Boolean> DEAD_TAG = Tag.Boolean("pve:dead").defaultValue(false);
    private static final Tag<Boolean> PLAYER_BLOCK_TAG = Tag.Boolean("pve:playerBlock").defaultValue(false);

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
            ChatUtil.apply(instance.eventNode(), gameConfig.getChatFormat(), player -> arena.getArenaFeature(DescriptionFeature.class).getReplacements());
            instance.eventNode()
                    .addChild(FoodConfig.DEFAULT.createNode())
                    .addChild(PotionConfig.DEFAULT.createNode())
                    .addChild(ExplosionConfig.DEFAULT.createNode())
                    .addChild(ArmorToolConfig.DEFAULT.createNode());

            InstanceEventHook.applyBow(instance.eventNode(), (entity, power) -> {
                final EntityProjectile projectile = new EntityProjectile(entity, EntityType.ARROW);
                final ArrowMeta meta = (ArrowMeta) projectile.getEntityMeta();
                meta.setCritical(power >= 0.9);
                projectile.scheduleRemove(Duration.of(100, TimeUnit.SERVER_TICK));
                return projectile;
            });
            InstanceEventHook.applyCombat(instance.eventNode(), false, (attacker, victim) -> {
                float damage = 1;
                if (attacker instanceof LivingEntity livingEntity) {
                    damage = livingEntity.getAttributeValue(Attribute.ATTACK_DAMAGE);
                } else if (attacker instanceof EntityProjectile projectile && projectile.getShooter() instanceof Player player) {
                    final float movementSpeed = (float) (projectile.getVelocity().length() / MinecraftServer.TICK_PER_SECOND);
                    damage = movementSpeed * player.getAttributeValue(Attribute.ATTACK_DAMAGE);
                }

                if (victim instanceof Player player) {
                    float armorPoints = player.getAttributeValue(Attribute.ARMOR);

                    // Armor point = 4% damage reduction
                    // 20 armor points = max reduction
                    /* * Math.pow(1.15, getUpgrade(ALLOYING_UPGRADE)) */
                    final float multi = -0.04f * armorPoints;

                    damage *= Math.max(1 + multi, 0.2);
                }

                return damage;
            }, victim -> {
                if (victim instanceof Player) return 500;
                else return 100;
            });

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
                    .addListener(EntityDamageEvent.class, event -> {
                        if (event.getEntity() instanceof Player player) {
                            var totalHealth = player.getHealth() + player.getAdditionalHearts();
                            var damage = event.getDamage();
                            if (totalHealth - damage <= 0) {
                                onKill(player);
                            }
                        }
                    })
                    .addListener(PlayerMoveEvent.class, event -> {
                        if (!instance.isInVoid(event.getNewPosition())) return;
                        event.setNewPosition(SPAWN_POS);
                        if (isInGame()) {
                            onKill(event.getPlayer());
                        }
                    })
                    .addListener(PlayerBlockBreakEvent.class, event -> {
                        if (Boolean.FALSE.equals(event.getBlock().getTag(PLAYER_BLOCK_TAG))) {
                            event.setCancelled(true);
                        }
                    })
                    .addListener(ExplosionEvent.class, event -> event.getAffectedBlocks().removeIf(point -> !Boolean.TRUE.equals(instance.getBlock(point).getTag(PLAYER_BLOCK_TAG))))
                    .addListener(PlayerBlockPlaceEvent.class, event -> event.setBlock(event.getBlock().withTag(PLAYER_BLOCK_TAG, true)));

            MinecraftServer.getInstanceManager().registerInstance(instance);
            MinecraftServer.getGlobalEventHandler().addChild(entityEventNode);
        }

        @Override
        public void clear() {
            arena.getFeature(LobbyFeature.class).send(instance.getPlayers());
            MinecraftServer.getInstanceManager().unregisterInstance(instance);
            MinecraftServer.getGlobalEventHandler().removeChild(entityEventNode);
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
