package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.utils.NamespaceID;
import net.minestom.server.world.DimensionType;

/**
 * The utility class for full bright dimension
 */
@UtilityClass
public final class FullBrightDimension {
    /**
     * The full bright dimension type
     */
    public static final DimensionType INSTANCE = DimensionType.builder(NamespaceID.from("minestom:full_bright"))
            .skylightEnabled(true)
            .ambientLight(1.0f)
            .fixedTime(6000L)
            .build();

    static {
        MinecraftServer.getDimensionTypeManager().addDimension(INSTANCE);
    }
}
