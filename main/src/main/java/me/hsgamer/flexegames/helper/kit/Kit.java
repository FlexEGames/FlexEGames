package me.hsgamer.flexegames.helper.kit;

import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.Map;

/**
 * The kit interface including the display name, display item, and items
 */
public interface Kit {
    /**
     * Get the display name of the kit
     *
     * @return the display name
     */
    Component getDisplayName();

    /**
     * Get the display item of the kit
     *
     * @return the display item
     */
    ItemStack getDisplayItem();

    /**
     * Get the items of the kit
     *
     * @return the items
     */
    Map<Integer, ItemStack> getItems();
}
