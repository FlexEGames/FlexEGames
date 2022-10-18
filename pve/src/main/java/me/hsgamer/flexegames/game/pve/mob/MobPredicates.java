package me.hsgamer.flexegames.game.pve.mob;

import lombok.experimental.UtilityClass;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;

import java.util.function.Predicate;

@UtilityClass
final class MobPredicates {
    static Predicate<Entity> playerExcludeSpectator() {
        return entity -> entity instanceof Player player && player.getGameMode() != GameMode.SPECTATOR;
    }
}
