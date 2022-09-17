package me.hsgamer.flexegames.api.game;

import me.hsgamer.minigamecore.base.Initializer;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.Collection;
import java.util.UUID;

public interface ArenaGame extends Initializer {
    Template getTemplate();

    ItemStack getDisplayItem();

    Collection<Player> getPlayers();

    default boolean isInGame(Player player) {
        return getPlayers().contains(player);
    }

    default boolean isInGame(UUID uuid) {
        var player = MinecraftServer.getConnectionManager().getPlayer(uuid);
        return player != null && isInGame(player);
    }

    default int getPlayerCount() {
        return getPlayers().size();
    }

    default JoinResponse join(Player player) {
        return JoinResponse.INCOMPLETE_SETUP;
    }

    default void onWaitingStart() {
        // EMPTY
    }

    default boolean isWaitingOver() {
        return true;
    }

    default boolean canStart() {
        return false;
    }

    default void onWaitingTick() {
        // EMPTY
    }

    default void onWaitingEnd() {
        // EMPTY
    }

    default void onFailedWaitingEnd() {
        // EMPTY
    }

    default void onInGameStart() {
        // EMPTY
    }

    default boolean isInGameOver() {
        return true;
    }

    default void onInGameTick() {
        // EMPTY
    }

    default void onInGameOver() {
        // EMPTY
    }

    default void onEndingStart() {
        // EMPTY
    }

    default boolean isEndingOver() {
        return true;
    }

    default void onEndingTick() {
        // EMPTY
    }

    default void onEndingOver() {
        // EMPTY
    }
}
