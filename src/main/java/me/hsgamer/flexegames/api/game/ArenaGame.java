package me.hsgamer.flexegames.api.game;

import me.hsgamer.minigamecore.base.Initializer;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

public interface ArenaGame extends Initializer {
    Template getTemplate();

    ItemStack getDisplayItem();

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

    default void onInGameOver() {
        // EMPTY
    }

    default void onEndingStart() {
        // EMPTY
    }

    default boolean isEndingOver() {
        return true;
    }

    default void onEndingOver() {
        // EMPTY
    }
}
