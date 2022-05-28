package me.hsgamer.flexegames.inventory;

import net.minestom.server.inventory.Inventory;

import java.util.Map;

public interface ButtonMap {
    Map<Integer, Button> getButtons(Inventory inventory);
}
