package me.hsgamer.flexegames.helper.kit;

import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.Map;

public interface GameKit {
    Component getDisplayName();

    ItemStack getDisplayItem();

    Map<Integer, ItemStack> getItems();
}
