package me.hsgamer.flexegames.feature;

import lombok.Getter;
import lombok.Setter;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;

import java.util.Optional;
import java.util.UUID;

public class OwnerFeature extends ArenaFeature<OwnerFeature.ArenaOwnerFeature> {
    @Override
    protected ArenaOwnerFeature createFeature(Arena arena) {
        return new ArenaOwnerFeature();
    }

    @Getter
    @Setter
    public static class ArenaOwnerFeature implements Feature {
        private UUID owner;

        public Component getDisplayOwner() {
            return Optional.ofNullable(owner)
                    .map(MinecraftServer.getConnectionManager()::getPlayer)
                    .map(Player::getName)
                    .orElseGet(Component::empty);
        }
    }
}
