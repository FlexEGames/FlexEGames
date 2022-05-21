package me.hsgamer.flexegames.util;

import io.github.togar2.blocks.MinestomBlocks;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.trait.EntityEvent;

@UtilityClass
public final class BlockUtil {
    public static EventNode<EntityEvent> applyBlockEvents(EventNode<EntityEvent> node) {
        return node.addChild(MinestomBlocks.events());
    }
}
