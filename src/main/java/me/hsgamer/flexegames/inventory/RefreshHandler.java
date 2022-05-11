package me.hsgamer.flexegames.inventory;

import net.minestom.server.inventory.Inventory;

public interface RefreshHandler {
    boolean canRefresh(Inventory inventory, boolean firstTime);
}
