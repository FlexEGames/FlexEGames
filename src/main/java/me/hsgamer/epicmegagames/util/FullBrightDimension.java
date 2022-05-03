package me.hsgamer.epicmegagames.util;

import net.minestom.server.MinecraftServer;
import net.minestom.server.utils.NamespaceID;
import net.minestom.server.world.DimensionType;

public final class FullBrightDimension {
    public static final DimensionType INSTANCE = DimensionType.builder(NamespaceID.from("minestom:full_bright"))
            .ambientLight(2.0f)
            .build();

    static {
        MinecraftServer.getDimensionTypeManager().addDimension(INSTANCE);
    }
}
