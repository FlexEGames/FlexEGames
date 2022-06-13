package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.event.ArenaLeaveEvent;
import me.hsgamer.flexegames.event.ArenaPreJoinEvent;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventDispatcher;

import java.util.Optional;

@UtilityClass
public final class ArenaUtil {
    public static Optional<Player> getOwner(Arena arena) {
        return Optional.ofNullable(arena.getArenaFeature(GameFeature.class).getOwner())
                .map(uuid -> MinecraftServer.getConnectionManager().getPlayer(uuid));
    }

    public static Component getDisplayOwner(Arena arena) {
        return getOwner(arena).map(Player::getName).orElse(Component.empty());
    }

    public static Component getDisplayState(Arena arena) {
        return arena.getStateInstance()
                .map(GameState::getDisplayName)
                .map(LegacyComponentSerializer.legacyAmpersand()::deserialize)
                .orElse(Component.empty());
    }

    public static ArenaPreJoinEvent callPreJoinEvent(Arena arena, Player player) {
        ArenaPreJoinEvent event = new ArenaPreJoinEvent(arena, player);
        EventDispatcher.call(event);
        return event;
    }

    public static void callJoinEvent(Arena arena, Player player) {
        EventDispatcher.call(new ArenaLeaveEvent(arena, player));
    }

    public static void callLeaveEvent(Arena arena, Player player) {
        EventDispatcher.call(new ArenaLeaveEvent(arena, player));
    }
}
