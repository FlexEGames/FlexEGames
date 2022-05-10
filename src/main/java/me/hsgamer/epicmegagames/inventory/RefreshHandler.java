package me.hsgamer.epicmegagames.inventory;

import net.minestom.server.inventory.Inventory;

public interface RefreshHandler {
    boolean canRefresh(Inventory inventory, boolean firstTime);
}
