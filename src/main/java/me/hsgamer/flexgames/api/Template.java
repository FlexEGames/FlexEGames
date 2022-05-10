package me.hsgamer.flexgames.api;

import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.List;

public interface Template {
    ArenaGame createGame(Arena arena);

    Component getDisplayName();

    List<Component> getDescription();

    default ItemStack getDisplayItem() {
        return ItemStack.of(Material.STONE);
    }
}
