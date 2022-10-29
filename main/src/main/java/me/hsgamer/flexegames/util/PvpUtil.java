package me.hsgamer.flexegames.util;

import io.github.bloepiloepi.pvp.PvpExtension;
import io.github.bloepiloepi.pvp.explosion.PvpExplosionSupplier;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;

/**
 * The utility class for PVP
 */
@UtilityClass
public final class PvpUtil {
    /**
     * Register the PVP extension to the instance event node
     *
     * @param node   the event node
     * @param legacy whether to use legacy PVP
     */
    public static void applyPvp(EventNode<InstanceEvent> node, boolean legacy) {
        node.addChild(legacy ? PvpExtension.legacyEvents() : PvpExtension.events());
    }

    /**
     * Register the explosion supplier to the instance
     *
     * @param instance the instance
     */
    public static void applyExplosion(Instance instance) {
        instance.setExplosionSupplier(PvpExplosionSupplier.INSTANCE);
    }
}
