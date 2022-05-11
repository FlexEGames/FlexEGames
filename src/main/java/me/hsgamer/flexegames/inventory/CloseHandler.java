package me.hsgamer.flexegames.inventory;

import net.minestom.server.entity.Player;

import java.util.function.Predicate;

/**
 * The handler for closing the menu.
 * Return true to close the menu, false to keep it open.
 */
public interface CloseHandler extends Predicate<Player> {
}
