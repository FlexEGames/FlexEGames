package me.hsgamer.epicmegagames.api;

import me.hsgamer.minigamecore.base.Arena;

public abstract class GameTemplate {
    protected final Arena arena;

    protected GameTemplate(Arena arena) {
        this.arena = arena;
    }
}
