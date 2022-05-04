package me.hsgamer.epicmegagames.api;

import me.hsgamer.minigamecore.base.Initializer;
import net.minestom.server.entity.Player;

public interface ArenaGame extends Initializer {
    default JoinResponse join(Player player) {
        return JoinResponse.INCOMPLETE_SETUP;
    }

    default void onWaitingStart() {
        // EMPTY
    }

    default boolean isWaitingEnd() {
        return true;
    }

    default boolean canStart() {
        return false;
    }

    default void onWaitingEnd() {
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
