package me.hsgamer.flexegames.feature.arena;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.item.ItemStack;

import java.util.Map;
import java.util.function.Supplier;

/**
 * The feature to show the description of the {@link Arena}
 */
public interface DescriptionFeature extends Feature {
    /**
     * Get the replacement map of the {@link Arena}
     *
     * @return the replacement map
     */
    Map<String, Supplier<ComponentLike>> getReplacements();

    /**
     * Get the display name of the {@link Arena}
     *
     * @return the display name
     */
    ItemStack getDisplayItem();
}
