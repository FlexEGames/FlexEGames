package me.hsgamer.flexegames.template.duel.feature;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;

public class BoardFeature extends ArenaFeature<ArenaBoardFeature> {
    @Override
    protected ArenaBoardFeature createFeature(Arena arena) {
        return new ArenaBoardFeature(arena);
    }
}
