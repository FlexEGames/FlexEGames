package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;

import java.util.Optional;

@UtilityClass
public final class ArenaUtil {
    public static Component getOwner(Arena arena) {
        return Optional.ofNullable(arena.getArenaFeature(GameFeature.class).getOwner())
                .map(uuid -> MinecraftServer.getConnectionManager().getPlayer(uuid))
                .map(Player::getName)
                .orElse(Component.empty());
    }

    public static Component getState(Arena arena) {
        return arena.getStateInstance()
                .map(GameState::getDisplayName)
                .map(LegacyComponentSerializer.legacyAmpersand()::deserialize)
                .orElse(Component.empty());
    }
}
