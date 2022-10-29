package me.hsgamer.flexegames.util;

import net.minestom.server.instance.AnvilLoader;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.Instance;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Path;

/**
 * The type of chunk loader
 */
public enum ChunkLoaderType {
    /**
     * The anvil chunk loader
     */
    ANVIL((instance, path, readOnly) -> new AnvilLoader(path));

    private final ChunkLoaderProvider provider;

    ChunkLoaderType(ChunkLoaderProvider provider) {
        this.provider = provider;
    }

    /**
     * Create a chunk loader
     *
     * @param instance the instance
     * @param path     the world path
     * @param readOnly whether the chunk loader is read-only
     * @return the chunk loader
     */
    @Nullable
    public IChunkLoader getLoader(Instance instance, Path path, boolean readOnly) {
        return provider.getLoader(instance, path, readOnly);
    }

    /**
     * Create a chunk loader
     *
     * @param instance the instance
     * @param path     the world path
     * @return the chunk loader
     */
    @Nullable
    public IChunkLoader getLoader(Instance instance, Path path) {
        return getLoader(instance, path, false);
    }

    /**
     * Create a chunk loader
     *
     * @param instance the instance
     * @param path     the world path
     * @param readOnly whether the chunk loader is read-only
     * @return the chunk loader
     */
    @Nullable
    public IChunkLoader getLoader(Instance instance, String path, boolean readOnly) {
        return getLoader(instance, Path.of(path), readOnly);
    }

    /**
     * Create a chunk loader
     *
     * @param instance the instance
     * @param path     the world path
     * @return the chunk loader
     */
    @Nullable
    public IChunkLoader getLoader(Instance instance, String path) {
        return getLoader(instance, Path.of(path));
    }

    private interface ChunkLoaderProvider {
        /**
         * Create a chunk loader
         *
         * @param instance the instance
         * @param path     the world path
         * @param readOnly whether the chunk loader is read-only
         * @return the chunk loader
         */
        @Nullable
        IChunkLoader getLoader(Instance instance, Path path, boolean readOnly);
    }
}
