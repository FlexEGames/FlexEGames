package me.hsgamer.flexegames.api.game;

import net.kyori.adventure.text.Component;

/**
 * The response of joining a game
 *
 * @param success true if the player can join the game
 * @param message the message if the player cannot join the game
 */
public record JoinResponse(boolean success, Component message) {
    public static JoinResponse fail(Component message) {
        return new JoinResponse(false, message);
    }

    public static JoinResponse successful() {
        return new JoinResponse(true, Component.empty());
    }
}
