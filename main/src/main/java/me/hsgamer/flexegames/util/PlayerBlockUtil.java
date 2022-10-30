package me.hsgamer.flexegames.util;

import io.github.bloepiloepi.pvp.events.ExplosionEvent;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.tag.Tag;

/**
 * The utility class to handle player block.
 * This will prevent players from blocks that are not placed by players.
 */
@UtilityClass
public class PlayerBlockUtil {
    /**
     * The tag to check if the block is placed by a player
     */
    public static final Tag<Boolean> PLAYER_BLOCK_TAG = Tag.Boolean("games:playerBlock").defaultValue(false);

    /**
     * Register the event to handle player block
     *
     * @param node the event node
     */
    public static void apply(EventNode<InstanceEvent> node) {
        node.addListener(PlayerBlockBreakEvent.class, event -> {
            if (Boolean.FALSE.equals(event.getBlock().getTag(PLAYER_BLOCK_TAG))) {
                event.setCancelled(true);
            }
        });
        node.addListener(ExplosionEvent.class, event -> event.getAffectedBlocks().removeIf(point -> !Boolean.TRUE.equals(event.getInstance().getBlock(point).getTag(PLAYER_BLOCK_TAG))));
        node.addListener(PlayerBlockPlaceEvent.class, event -> event.setBlock(event.getBlock().withTag(PLAYER_BLOCK_TAG, true)));
    }
}
