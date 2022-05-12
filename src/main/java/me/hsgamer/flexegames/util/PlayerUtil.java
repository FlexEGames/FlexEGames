package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.entity.Player;

@UtilityClass
public final class PlayerUtil {
    public static void reset(Player player) {
        player.getInventory().clear();
        player.clearEffects();
        player.heal();
        player.setFood(20);
        player.setLevel(0);
        player.setAllowFlying(false);
        player.setFlying(false);
        player.setInvisible(false);
        player.setAdditionalHearts(0);
        player.setNoGravity(false);
        player.setExp(0);
        player.stopSpectating();
        player.setEnableRespawnScreen(false);
        player.updateViewerRule();
        player.updateViewableRule();
        player.askSynchronization();
    }
}
