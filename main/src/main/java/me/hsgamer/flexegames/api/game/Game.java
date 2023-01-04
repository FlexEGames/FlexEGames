package me.hsgamer.flexegames.api.game;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.List;
import java.util.function.Function;

public interface Game {
    /**
     * Check if the game is configured
     *
     * @return true if configured
     */
    boolean isConfigured();

    /**
     * Get the display name of the {@link Game}
     *
     * @return the display name
     */
    Component getDisplayName();

    /**
     * Get the description of the {@link Game}
     *
     * @return the description
     */
    List<Component> getDescription();

    /**
     * Get the display item of the {@link Game}
     *
     * @return the display item
     */
    ItemStack getDisplayItem();

    /**
     * Get the {@link Arena} of the {@link Game}
     *
     * @param name the name of the {@link Arena}
     * @return the function to create the {@link Arena}
     */
    Function<ArenaManager, Arena> createArena(String name);
}
