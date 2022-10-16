package me.hsgamer.flexegames.feature;

import lombok.Getter;
import lombok.Setter;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;

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
    }
}
