package me.hsgamer.epicmegagames.inventory;

import net.minestom.server.item.ItemStack;

public interface Button {
    ItemStack getItem();

    default ClickConsumer getClickConsumer() {
        return (player, clickType, result) -> {
            result.setCancel(true);
            return false;
        };
    }

    default boolean ignoreCancelled() {
        return true;
    }
}
