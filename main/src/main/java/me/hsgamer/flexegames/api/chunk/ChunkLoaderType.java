package me.hsgamer.flexegames.api.chunk;

import net.minestom.server.instance.AnvilLoader;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.Instance;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Path;

public enum ChunkLoaderType {
    ANVIL((instance, path, readOnly) -> new AnvilLoader(path));

    private final ChunkLoaderProvider provider;

    ChunkLoaderType(ChunkLoaderProvider provider) {
        this.provider = provider;
    }

    @Nullable
    public IChunkLoader getLoader(Instance instance, Path path, boolean readOnly) {
        return provider.getLoader(instance, path, readOnly);
    }

    @Nullable
    public IChunkLoader getLoader(Instance instance, Path path) {
        return getLoader(instance, path, false);
    }

    @Nullable
    public IChunkLoader getLoader(Instance instance, String path, boolean readOnly) {
        return getLoader(instance, Path.of(path), readOnly);
    }

    @Nullable
    public IChunkLoader getLoader(Instance instance, String path) {
        return getLoader(instance, Path.of(path));
    }

    private interface ChunkLoaderProvider {
        @Nullable
        IChunkLoader getLoader(Instance instance, Path path, boolean readOnly);
    }
}
