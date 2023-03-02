package me.hsgamer.flexegames.helper.kit;

import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.Map;

public interface Kit {
    Component getDisplayName();

    ItemStack getDisplayItem();

    Map<Integer, ItemStack> getItems();

    default void giveItems(Player player) {
        var inventory = player.getInventory();
        getItems().forEach((slot, item) -> {
            if (slot < 0 || slot >= inventory.getSize()) return;
            player.getInventory().setItemStack(slot, item);
        });
    }
}
