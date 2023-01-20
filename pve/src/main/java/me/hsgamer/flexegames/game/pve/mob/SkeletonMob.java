package me.hsgamer.flexegames.game.pve.mob;

import io.github.bloepiloepi.pvp.projectile.Arrow;
import me.hsgamer.flexegames.game.pve.feature.ConfigFeature;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.coordinate.Vec;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.EntityType;
import net.minestom.server.entity.ai.goal.RangedAttackGoal;
import net.minestom.server.entity.ai.target.ClosestEntityTarget;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;
import net.minestom.server.utils.time.TimeUnit;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.util.List;

public final class SkeletonMob extends ArenaMob {
    public SkeletonMob(Arena arena) {
        super(EntityType.SKELETON, arena);
        setItemInMainHand(ItemStack.of(Material.BOW));

        RangedAttackGoal rangedAttackGoal = new RangedAttackGoal(
                this, Duration.of(40, TimeUnit.SERVER_TICK),
                16, 8, false, 1, 0.1);

        var gameConfig = arena.getFeature(ConfigFeature.class).config();
        rangedAttackGoal.setProjectileGenerator((shooter, target, power, spread) -> {
            HomingArrow projectile = new HomingArrow(shooter, EntityType.PLAYER, gameConfig.isUseLegacyPvp());
            projectile.scheduleRemove(Duration.of(100, TimeUnit.SERVER_TICK));
            projectile.setInstance(shooter.getInstance(), shooter.getPosition().add(0D, shooter.getEyeHeight(), 0D));
            projectile.shoot(target, power, spread);
        });

        addAIGroup(
                List.of(rangedAttackGoal),
                List.of(new ClosestEntityTarget(this, 32, MobPredicates.playerExcludeSpectator()))
        );
    }

    private static final class HomingArrow extends Arrow {
        private final EntityType target;

        public HomingArrow(@Nullable Entity shooter, EntityType target, boolean isLegacy) {
            super(shooter, isLegacy);
            this.target = target;
        }

        @Override
        public void tick(long time) {
            super.tick(time);
            if (instance == null) return;
            if (isOnGround()) return;

            for (Entity entity : instance.getNearbyEntities(position, 5.0)) {
                if (entity.getEntityType() != target) continue;

                final Vec target = position.withLookAt(entity.getPosition()).direction();
                final Vec newVelocity = velocity.add(target);

                setVelocity(newVelocity);

                return;
            }
        }
    }
}
