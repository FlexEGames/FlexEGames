package me.hsgamer.flexegames.util;

import io.github.bloepiloepi.pvp.PvpExtension;
import io.github.bloepiloepi.pvp.entities.EntityUtils;
import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.coordinate.Vec;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.entity.EntityTickEvent;
import net.minestom.server.event.player.PlayerMoveEvent;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.potion.PotionEffect;

@UtilityClass
public final class PvpUtil {
    public static EventNode<EntityEvent> applyPvp(EventNode<EntityEvent> node, boolean legacy) {
        return node
                .addChild(legacy ? PvpExtension.legacyEvents() : PvpExtension.events())
                .addListener(EntityTickEvent.class, event -> {
                    final int tps = MinecraftServer.TICK_PER_SECOND;
                    final Entity entity = event.getEntity();
                    final Vec velocity = entity.getVelocity();
                    if (EntityUtils.hasEffect(entity, PotionEffect.LEVITATION)) {
                        entity.setVelocity(entity.getVelocity().withY(
                                ((0.05 * (EntityUtils.getEffect(entity, PotionEffect.LEVITATION).amplifier() + 1) - (velocity.y() / tps)) * 0.2) * tps
                        ));
                    } else if (velocity.y() < 0 && EntityUtils.hasEffect(entity, PotionEffect.SLOW_FALLING)) {
                        entity.setVelocity(velocity.withY(velocity.y() - 0.01));
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    final Player player = event.getPlayer();
                    if (!EntityUtils.hasEffect(player, PotionEffect.JUMP_BOOST)) return;
                    final Pos from = player.getPosition();
                    final Pos to = event.getNewPosition();
                    final double yDiff = to.y() - from.y();
                    final int tps = MinecraftServer.TICK_PER_SECOND;
                    if (yDiff <= 0.0D || !player.isOnGround()) return;
                    Vec velocity = player.getVelocity();
                    double jumpY = 0.42;
                    jumpY += (EntityUtils.getEffect(player, PotionEffect.JUMP_BOOST).amplifier() + 1) * 0.1F;
                    velocity = velocity.withY(jumpY * tps);
                    if (player.isSprinting()) {
                        double angle = from.yaw() * (Math.PI / 180);
                        velocity = velocity.add(-Math.sin(angle) * 0.2 * tps, 0, Math.cos(angle) * 0.2 * tps);
                    }
                    player.setVelocity(velocity);
                });
    }
}
