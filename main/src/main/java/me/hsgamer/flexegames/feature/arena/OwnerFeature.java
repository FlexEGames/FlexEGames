package me.hsgamer.flexegames.feature.arena;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;

import java.util.Optional;
import java.util.UUID;

/**
 * The feature to get the owner of the specific {@link Arena}
 *
 * @param owner the owner of the {@link Arena}
 */
public record OwnerFeature(UUID owner) implements Feature {
    /**
     * Get the display name of the owner
     *
     * @return the display name
     */
    public Component getDisplayOwner() {
        return Optional.ofNullable(owner)
                .map(MinecraftServer.getConnectionManager()::getPlayer)
                .map(Player::getName)
                .orElseGet(Component::empty);
    }
}
