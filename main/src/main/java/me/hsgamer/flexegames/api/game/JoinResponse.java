package me.hsgamer.flexegames.api.game;

import net.kyori.adventure.text.Component;

public record JoinResponse(boolean success, Component message) {
    public static JoinResponse fail(Component message) {
        return new JoinResponse(false, message);
    }

    public static JoinResponse successful() {
        return new JoinResponse(true, Component.empty());
    }
}
