package me.hsgamer.flexegames.game.duel.feature;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;

public class InstanceFeature extends ArenaFeature<ArenaInstanceFeature> {
    @Override
    protected ArenaInstanceFeature createFeature(Arena arena) {
        return new ArenaInstanceFeature(arena);
    }
}
