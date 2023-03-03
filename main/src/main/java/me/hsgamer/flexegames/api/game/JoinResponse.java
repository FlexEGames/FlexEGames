package me.hsgamer.flexegames.api.game;

import net.kyori.adventure.text.Component;

/**
 * The response of joining a game
 *
 * @param success true if the player can join the game
 * @param message the message if the player cannot join the game
 */
public record JoinResponse(boolean success, Component message) {
    /**
     * Create a failed response
     *
     * @param message the message
     * @return the response
     */
    public static JoinResponse fail(Component message) {
        return new JoinResponse(false, message);
    }

    /**
     * Create a successful response
     *
     * @return the response
     */
    public static JoinResponse successful() {
        return new JoinResponse(true, Component.empty());
    }
}
