package me.hsgamer.flexegames.event;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.minestom.server.entity.Player;
import net.minestom.server.event.trait.CancellableEvent;
import net.minestom.server.event.trait.PlayerEvent;

@RequiredArgsConstructor
@Getter
public class ArenaPreJoinEvent implements PlayerEvent, CancellableEvent {
    private final Arena arena;
    private final Player player;
    @Setter
    private JoinResponse response = JoinResponse.SUCCESSFUL_JOIN;

    @Override
    public boolean isCancelled() {
        return response.success();
    }

    @Override
    public void setCancelled(boolean cancel) {
        response = cancel ? JoinResponse.of(false, Component.text("Cancelled").color(NamedTextColor.RED)) : JoinResponse.SUCCESSFUL_JOIN;
    }

    public void setResponseMessage(String message) {
        response = JoinResponse.of(response.success(), Component.text(message).color(NamedTextColor.RED));
    }
}
