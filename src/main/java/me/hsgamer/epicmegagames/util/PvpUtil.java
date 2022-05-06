package me.hsgamer.epicmegagames.util;

import io.github.bloepiloepi.pvp.PvpExtension;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.trait.EntityEvent;

@UtilityClass
public final class PvpUtil {
    public static EventNode<EntityEvent> applyPvp(EventNode<EntityEvent> node, boolean legacy) {
        return node.addChild(legacy ? PvpExtension.legacyEvents() : PvpExtension.events());
    }
}
