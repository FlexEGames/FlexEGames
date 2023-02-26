package me.hsgamer.flexegames.api.game;

import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.List;
import java.util.concurrent.CompletableFuture;

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

    CompletableFuture<GamePropertyMap> editProperty(Player player, GamePropertyMap gamePropertyMap);

    default CompletableFuture<GamePropertyMap> createProperty(Player player) {
        return editProperty(player, GamePropertyMap.create());
    }

    Arena create(String name, ArenaManager arenaManager, GamePropertyMap gamePropertyMap);
}
