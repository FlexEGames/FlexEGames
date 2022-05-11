package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;

import java.io.File;

@UtilityClass
public final class WorldUtil {
    private static final File worldFolder = new File("world");

    public static File getWorldFolder() {
        return worldFolder;
    }

    public static File getWorldFile(String worldName) {
        return new File(worldFolder, worldName);
    }

    static {
        if (!worldFolder.exists()) {
            worldFolder.mkdirs();
        } else if (!worldFolder.isDirectory()) {
            throw new IllegalStateException("world is not a directory");
        }
    }
}
