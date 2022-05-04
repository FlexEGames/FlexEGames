package me.hsgamer.epicmegagames.hook;

import me.hsgamer.epicmegagames.config.ChatConfig;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerChatEvent;

import java.util.Objects;

public class ChatHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerChatEvent.class, event -> {
            Component chatFormat = ChatConfig.CHAT_FORMAT.getValue();
            Player player = event.getPlayer();
            event.setChatFormat(e -> chatFormat
                    .replaceText(builder -> builder.match("%player%").replacement(e.getPlayer().getName()))
                    .replaceText(builder -> builder.match("%message%").replacement(e.getMessage()))
            );
            event.getRecipients().removeIf(p -> !Objects.equals(p.getInstance(), player.getInstance()));
        });
    }
}
