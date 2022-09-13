package me.hsgamer.flexegames.util;

import io.github.bloepiloepi.pvp.PvpExtension;
import io.github.bloepiloepi.pvp.explosion.PvpExplosionSupplier;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;

@UtilityClass
public final class PvpUtil {
    public static EventNode<InstanceEvent> applyPvp(EventNode<InstanceEvent> node, boolean legacy) {
        return node.addChild(legacy ? PvpExtension.legacyEvents() : PvpExtension.events());
    }

    public static void applyExplosion(Instance instance) {
        instance.setExplosionSupplier(PvpExplosionSupplier.INSTANCE);
    }
}
