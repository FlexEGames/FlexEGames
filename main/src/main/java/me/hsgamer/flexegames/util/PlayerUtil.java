package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.attribute.Attribute;
import net.minestom.server.entity.Player;

import java.util.Optional;
import java.util.UUID;

@UtilityClass
public final class PlayerUtil {
    public static void reset(Player player) {
        player.getInventory().clear();
        player.setAutoViewable(true);
        player.getEntityMeta().setNotifyAboutChanges(false);
        player.setInvisible(false);
        player.setGlowing(false);
        player.setAdditionalHearts(0);
        player.setAllowFlying(false);
        player.setFlying(false);
        player.setNoGravity(false);
        player.setFood(20);
        player.setLevel(0);
        player.setExp(0);
        player.setEnableRespawnScreen(false);
        player.stopSpectating();
        Optional.ofNullable(player.getVehicle()).ifPresent(vehicle -> vehicle.removePassenger(player));
        player.getAttribute(Attribute.MOVEMENT_SPEED).setBaseValue(0.1f);
        player.setCanPickupItem(true);
        player.setBoundingBox(0.6, 1.8, 0.6);
        player.closeInventory();
        player.refreshCommands();
        player.heal();
        player.clearEffects();
        player.getEntityMeta().setNotifyAboutChanges(true);
        player.askSynchronization();
        player.updateViewerRule(entity -> true);
        player.updateViewableRule(player1 -> true);
    }

    public static Player getPlayer(UUID uuid) {
        return MinecraftServer.getConnectionManager().getPlayer(uuid);
    }
}
