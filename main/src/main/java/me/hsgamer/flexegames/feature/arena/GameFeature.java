package me.hsgamer.flexegames.feature.arena;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.api.property.PropertyMap;
import me.hsgamer.minigamecore.base.Feature;

/**
 * The feature to access the {@link Game} of the {@link me.hsgamer.minigamecore.base.Arena}
 *
 * @param game the game
 */
public record GameFeature(Game game, PropertyMap propertyMap) implements Feature {
}
