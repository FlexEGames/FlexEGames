package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;

import java.io.File;

@UtilityClass
public final class AssetUtil {
    private static final File worldsFolder = new File("worlds");
    private static final File assetsFolder = new File("assets");

    static {
        if (!worldsFolder.exists()) {
            worldsFolder.mkdirs();
        } else if (!worldsFolder.isDirectory()) {
            throw new IllegalStateException("world is not a directory");
        }

        if (!assetsFolder.exists()) {
            assetsFolder.mkdirs();
        } else if (!assetsFolder.isDirectory()) {
            throw new IllegalStateException("assets is not a directory");
        }
    }

    public static File getWorldsFolder() {
        return worldsFolder;
    }

    public static File getWorldFile(String worldName) {
        return new File(worldsFolder, worldName);
    }

    public static File getAssetsFolder() {
        return assetsFolder;
    }

    public static File getAssetFile(String assetName) {
        return new File(assetsFolder, assetName);
    }
}
