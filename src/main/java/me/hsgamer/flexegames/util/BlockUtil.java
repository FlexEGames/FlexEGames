package me.hsgamer.flexegames.util;

import io.github.togar2.blocks.MinestomBlocks;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.trait.EntityEvent;

@UtilityClass
public final class BlockUtil {
    private static boolean initBlock = false;

    public static EventNode<EntityEvent> applyBlockEvents(EventNode<EntityEvent> node) {
        if (!initBlock) {
            MinestomBlocks.init();
            initBlock = true;
        }
        return node.addChild(MinestomBlocks.events());
    }
}
