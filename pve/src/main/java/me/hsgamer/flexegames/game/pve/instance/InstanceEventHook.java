package me.hsgamer.flexegames.game.pve.instance;

import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.coordinate.Vec;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.EntityProjectile;
import net.minestom.server.entity.LivingEntity;
import net.minestom.server.entity.Player;
import net.minestom.server.entity.damage.DamageType;
import net.minestom.server.entity.hologram.Hologram;
import net.minestom.server.event.EventListener;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.entity.EntityAttackEvent;
import net.minestom.server.event.entity.projectile.ProjectileCollideWithEntityEvent;
import net.minestom.server.event.item.ItemUpdateStateEvent;
import net.minestom.server.event.player.PlayerItemAnimationEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.item.Material;
import net.minestom.server.tag.Tag;
import net.minestom.server.utils.MathUtils;
import net.minestom.server.utils.time.TimeUnit;

import java.time.Duration;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.BiFunction;
import java.util.function.ToDoubleBiFunction;
import java.util.function.ToLongFunction;

@UtilityClass
public class InstanceEventHook {
    private static final Tag<Long> CHARGE_SINCE_TAG = Tag.Long("bow_charge_since").defaultValue(Long.MAX_VALUE);
    private static final Tag<Long> INVULNERABLE_UNTIL_TAG = Tag.Long("invulnerable_until").defaultValue(0L);

    public static void applyBow(EventNode<InstanceEvent> node, BiFunction<Entity, Double, EntityProjectile> projectileGenerator) {
        node.addListener(EventListener.builder(PlayerItemAnimationEvent.class)
                .handler(event -> event.getPlayer().setTag(CHARGE_SINCE_TAG, System.currentTimeMillis()))
                .filter(event -> event.getItemAnimationType() == PlayerItemAnimationEvent.ItemAnimationType.BOW)
                .build()
        ).addListener(EventListener.builder(ItemUpdateStateEvent.class)
                .handler(event -> {
                    final Player player = event.getPlayer();
                    final double chargedFor = (System.currentTimeMillis() - player.getTag(CHARGE_SINCE_TAG)) / 1000D;
                    final double power = MathUtils.clamp((chargedFor * chargedFor + 2 * chargedFor) / 2D, 0, 1);

                    if (power > 0.2) {
                        final EntityProjectile projectile = projectileGenerator.apply(player, power);
                        final Pos position = player.getPosition().add(0, player.getEyeHeight(), 0);

                        projectile.setInstance(Objects.requireNonNull(player.getInstance()), position);

                        Vec direction = projectile.getPosition().direction();
                        projectile.shoot(position.add(direction).sub(0, 0.2, 0), power * 3, 1.0);
                    }

                    // Restore arrow
                    player.getInventory().update();
                })
                .filter(event -> event.getItemStack().material() == Material.BOW)
                .build());
    }

    public static void applyCombat(EventNode<InstanceEvent> node, boolean playerCombat, ToDoubleBiFunction<Entity, Entity> damageFunction, ToLongFunction<Entity> invulnerabilityFunction) {
        node.addListener(ProjectileCollideWithEntityEvent.class, event -> {
            if (!(event.getTarget() instanceof LivingEntity target)) return;
            if (!(event.getEntity() instanceof EntityProjectile projectile)) return;

            // PVP is disabled and two players have attempted to hit each other
            if (!playerCombat && target instanceof Player && projectile.getShooter() instanceof Player) return;

            // Don't apply damage if entity is invulnerable
            final long now = System.currentTimeMillis();
            final long invulnerableUntil = target.getTag(INVULNERABLE_UNTIL_TAG);
            if (invulnerableUntil > now) return;

            float damage = (float) damageFunction.applyAsDouble(projectile, target);

            target.damage(DamageType.fromProjectile(projectile.getShooter(), projectile), damage);
            target.setTag(INVULNERABLE_UNTIL_TAG, now + invulnerabilityFunction.applyAsLong(target));

            takeKnockbackFromArrow(target, projectile);
            if (damage > 0) spawnHologram(target, damage);

            projectile.remove();
        }).addListener(EntityAttackEvent.class, event -> {
            if (!(event.getTarget() instanceof LivingEntity target)) return;

            // PVP is disabled and two players have attempted to hit each other
            if (!playerCombat && target instanceof Player && event.getEntity() instanceof Player) return;

            // Can't have dead sources attacking things
            if (((LivingEntity) event.getEntity()).isDead()) return;

            // Don't apply damage if entity is invulnerable
            final long now = System.currentTimeMillis();
            final long invulnerableUntil = target.getTag(INVULNERABLE_UNTIL_TAG);
            if (invulnerableUntil > now) return;

            float damage = (float) damageFunction.applyAsDouble(event.getEntity(), target);

            target.damage(DamageType.fromEntity(event.getEntity()), damage);
            target.setTag(INVULNERABLE_UNTIL_TAG, now + invulnerabilityFunction.applyAsLong(target));

            takeKnockback(target, event.getEntity());
            if (damage > 0) spawnHologram(target, damage);
        });
    }

    private static void takeKnockback(Entity target, Entity source) {
        target.takeKnockback(
                0.3f,
                Math.sin(source.getPosition().yaw() * (Math.PI / 180)),
                -Math.cos(source.getPosition().yaw() * (Math.PI / 180))
        );
    }

    private static void takeKnockbackFromArrow(Entity target, EntityProjectile source) {
        if (source.getShooter() == null) return;
        takeKnockback(target, source.getShooter());
    }

    private static void spawnHologram(Entity target, float damage) {
        damage = MathUtils.round(damage, 2);

        new DamageHologram(
                target.getInstance(),
                target.getPosition().add(0, target.getEyeHeight(), 0),
                Component.text(damage, NamedTextColor.RED)
        );
    }

    private static final class DamageHologram extends Hologram {
        private DamageHologram(Instance instance, Pos spawnPosition, Component text) {
            super(instance, spawnPosition, text, true, true);
            getEntity().getEntityMeta().setHasNoGravity(false);

            Random random = ThreadLocalRandom.current();
            getEntity().setVelocity(getPosition()
                    .direction()
                    .withX(random.nextDouble(2))
                    .withY(3)
                    .withZ(random.nextDouble(2))
                    .normalize().mul(3));

            getEntity().scheduleRemove(Duration.of(15, TimeUnit.SERVER_TICK));
        }
    }
}
