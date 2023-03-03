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

/**
 * The game
 */
public interface Game {
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
     * Edit the property of the {@link Game}
     *
     * @param player      the involved player
     * @param propertyMap the property map
     * @return the future that contains the edited property map
     */
    CompletableFuture<PropertyMap> editProperty(Player player, PropertyMap propertyMap);

    /**
     * Create the property of the {@link Game}
     *
     * @param player the involved player
     * @return the future that contains the created property map
     */
    default CompletableFuture<PropertyMap> createProperty(Player player) {
        return editProperty(player, PropertyMap.create());
    }

    /**
     * Create the {@link Arena} of the {@link Game}
     *
     * @param name         the name of the arena
     * @param propertyMap  the property map
     * @param arenaManager the arena manager
     * @param owner        the owner of the arena
     * @return the created arena
     */
    Arena create(String name, PropertyMap propertyMap, ArenaManager arenaManager, UUID owner);
}
