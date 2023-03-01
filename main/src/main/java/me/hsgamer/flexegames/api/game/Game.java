package me.hsgamer.flexegames.api.game;

import me.hsgamer.flexegames.api.property.PropertyMap;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.List;
import java.util.UUID;
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

    CompletableFuture<PropertyMap> editProperty(Player player, PropertyMap propertyMap);

    default CompletableFuture<PropertyMap> createProperty(Player player) {
        return editProperty(player, PropertyMap.create());
    }

    Arena create(String name, PropertyMap propertyMap, ArenaManager arenaManager, UUID owner);
}
