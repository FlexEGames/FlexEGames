package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;

import java.io.File;

/**
 * The utility class for assets
 */
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

    /**
     * Get the folder for worlds
     *
     * @return the folder
     */
    public static File getWorldsFolder() {
        return worldsFolder;
    }

    /**
     * Get the world file/folder
     *
     * @param worldName the name of the world
     * @return the file/folder
     */
    public static File getWorldFile(String worldName) {
        return new File(worldsFolder, worldName);
    }

    /**
     * Get the folder for assets
     *
     * @return the folder
     */
    public static File getAssetsFolder() {
        return assetsFolder;
    }

    /**
     * Get the asset file/folder
     *
     * @param assetName the name of the asset
     * @return the file/folder
     */
    public static File getAssetFile(String assetName) {
        return new File(assetsFolder, assetName);
    }
}
