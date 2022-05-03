package me.hsgamer.epicmegagames.api;

import me.hsgamer.minigamecore.base.Arena;

public interface GameTemplateProvider {
    GameTemplate createTemplate(Arena arena);
}
