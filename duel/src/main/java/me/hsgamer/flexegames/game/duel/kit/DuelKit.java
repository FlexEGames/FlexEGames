package me.hsgamer.flexegames.game.duel.kit;

import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.Map;

public interface DuelKit {
    Component getDisplayName();

    ItemStack getDisplayItem();

    Map<Integer, ItemStack> getItems();
}
