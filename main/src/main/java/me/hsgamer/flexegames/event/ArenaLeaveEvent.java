package me.hsgamer.flexegames.event;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.entity.Player;
import net.minestom.server.event.trait.PlayerEvent;

@RequiredArgsConstructor
@Getter
@SuppressWarnings("ClassCanBeRecord")
public final class ArenaLeaveEvent implements PlayerEvent {
    private final Arena arena;
    private final Player player;
}
