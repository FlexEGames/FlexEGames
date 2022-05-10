package me.hsgamer.flexgames.api;

import me.hsgamer.flexgames.config.MessageConfig;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

import java.util.function.Function;

public record JoinResponse(boolean success, Function<Player, Component> message) {
    public static final JoinResponse SUCCESSFUL_JOIN = new JoinResponse(true, player -> Component.empty());
    public static final JoinResponse MAX_PLAYER_REACHED = new JoinResponse(false, player -> MessageConfig.RESPONSE_MAX_PLAYERS_REACH.getValue());
    public static final JoinResponse NOT_WAITING = new JoinResponse(false, player -> MessageConfig.RESPONSE_NOT_WAITING.getValue());
    public static final JoinResponse INCOMPLETE_SETUP = new JoinResponse(false, player -> MessageConfig.RESPONSE_INCOMPLETE_SETUP.getValue());

    public static JoinResponse of(boolean success, Component component) {
        return new JoinResponse(success, player -> component);
    }

    public Component getMessage(Player player) {
        return message.apply(player);
    }
}
