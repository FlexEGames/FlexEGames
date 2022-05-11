package me.hsgamer.flexegames.inventory;

import net.minestom.server.entity.Player;
import net.minestom.server.inventory.click.ClickType;
import net.minestom.server.inventory.condition.InventoryConditionResult;

public interface ClickConsumer {
    /**
     * Called when the player click the item
     *
     * @param player    the player
     * @param clickType the click type
     * @param result    the result
     * @return true if the inventory should be refreshed
     */
    boolean onClick(Player player, ClickType clickType, InventoryConditionResult result);
}
