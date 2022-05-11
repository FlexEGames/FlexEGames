package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.config.ChatConfig;
import me.hsgamer.flexegames.manager.ReplacementManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerChatEvent;

import java.util.Map;
import java.util.Objects;

@UtilityClass
public final class ChatHook {
    public static void hook(EventNode<Event> node) {
        node.addListener(PlayerChatEvent.class, event -> {
            event.setChatFormat(e -> ReplacementManager.builder()
                    .replaceGlobal()
                    .replacePlayer(event.getPlayer())
                    .replace(Map.of("message", () -> Component.text(e.getMessage())))
                    .build(ChatConfig.CHAT_FORMAT.getValue())
            );
            event.getRecipients().removeIf(p -> !Objects.equals(p.getInstance(), event.getPlayer().getInstance()));
        });
    }
}
