package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import net.minestom.server.utils.NamespaceID;
import net.minestom.server.world.DimensionType;

@UtilityClass
public final class FullBrightDimension {
    public static final DimensionType INSTANCE = DimensionType.builder(NamespaceID.from("minestom:full_bright"))
            .skylightEnabled(true)
            .ambientLight(2.0f)
            .build();

    static {
        MinecraftServer.getDimensionTypeManager().addDimension(INSTANCE);
    }
}
