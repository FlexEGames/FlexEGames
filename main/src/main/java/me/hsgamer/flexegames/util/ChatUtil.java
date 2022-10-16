package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.manager.ReplacementManager;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerChatEvent;
import net.minestom.server.event.trait.InstanceEvent;

import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

@UtilityClass
public final class ChatUtil {
    public static void apply(EventNode<InstanceEvent> node, Component chatFormat, Function<Player, Map<String, Supplier<ComponentLike>>> playerReplacementSupplier) {
        node.addListener(PlayerChatEvent.class, event -> {
            event.setChatFormat(e -> ReplacementManager.builder()
                    .replaceGlobal()
                    .replacePlayer(event.getPlayer())
                    .replace(playerReplacementSupplier.apply(event.getPlayer()))
                    .replace(Map.of("message", () -> Component.text(e.getMessage())))
                    .build(chatFormat)
            );
            event.getRecipients().removeIf(p -> !Objects.equals(p.getInstance(), event.getPlayer().getInstance()));
        });
    }

    public static void apply(EventNode<InstanceEvent> node, Component chatFormat) {
        apply(node, chatFormat, p -> Map.of());
    }
}
