package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;

import java.util.Optional;

@UtilityClass
public final class TemplateUtil {
    public static Component getOwner(Arena arena) {
        return Optional.ofNullable(arena.getArenaFeature(GameFeature.class).getOwner())
                .map(uuid -> MinecraftServer.getConnectionManager().getPlayer(uuid))
                .map(Player::getName)
                .orElse(Component.empty());
    }
}
