package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.attribute.Attribute;
import net.minestom.server.entity.Player;

import java.util.Optional;
import java.util.UUID;

/**
 * The utility class for {@link Player}
 */
@UtilityClass
public final class PlayerUtil {
    /**
     * Reset the player's attributes
     *
     * @param player the player
     */
    public static void reset(Player player) {
        player.getInventory().clear();
        player.clearEffects();
        player.setAutoViewable(true);
        player.setFireForDuration(0);
        player.setInvulnerable(false);
        player.getEntityMeta().setNotifyAboutChanges(false);
        player.setInvisible(false);
        player.setGlowing(false);
        player.setAdditionalHearts(0);
        player.setAllowFlying(false);
        player.setFlying(false);
        player.setNoGravity(false);
        player.setFood(20);
        player.setFoodSaturation(5);
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
        player.setOnFire(false);
        player.getEntityMeta().setNotifyAboutChanges(true);
        player.askSynchronization();
        player.updateViewerRule(entity -> true);
        player.updateViewableRule(player1 -> true);
        player.resetTitle();
    }

    /**
     * Get the player by its unique id
     *
     * @param uuid the unique id
     * @return the player
     */
    public static Player getPlayer(UUID uuid) {
        return MinecraftServer.getConnectionManager().getPlayer(uuid);
    }
}
