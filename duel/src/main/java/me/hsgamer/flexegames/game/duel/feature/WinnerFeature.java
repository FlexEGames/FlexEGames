package me.hsgamer.flexegames.game.duel.feature;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;

public class WinnerFeature extends ArenaFeature<ArenaWinnerFeature> {
    @Override
    protected ArenaWinnerFeature createFeature(Arena arena) {
        return new ArenaWinnerFeature(arena);
    }
}
