package me.hsgamer.flexegames.event;

import lombok.Getter;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.entity.Player;
import net.minestom.server.event.trait.PlayerEvent;

public record ArenaJoinEvent(Arena arena, @Getter Player player) implements PlayerEvent {
}
