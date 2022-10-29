package me.hsgamer.flexegames.feature;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Feature;

/**
 * The feature to access the {@link GameServer}
 *
 * @param gameServer the {@link GameServer}
 */
public record GameServerFeature(GameServer gameServer) implements Feature {
}
