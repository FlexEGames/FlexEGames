package me.hsgamer.epicmegagames.inventory;

import net.minestom.server.entity.Player;
import net.minestom.server.inventory.click.ClickType;
import net.minestom.server.inventory.condition.InventoryConditionResult;

public interface ClickConsumer {
    void onClick(Player player, ClickType clickType, InventoryConditionResult result);
}
