package me.hsgamer.flexgames.inventory;

import net.minestom.server.entity.Player;

import java.util.function.Predicate;

/**
 * The handler for opening the menu.
 * Return true to open the menu, false to cancel it.
 */
public interface OpenHandler extends Predicate<Player> {
}
