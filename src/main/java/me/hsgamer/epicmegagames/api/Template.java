package me.hsgamer.epicmegagames.api;

import me.hsgamer.minigamecore.base.Initializer;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

public interface Template extends Initializer {
    default JoinResponse join(Player player) {
        return new JoinResponse(false, Component.text("You can't join this game"));
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
