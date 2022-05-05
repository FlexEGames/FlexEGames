package me.hsgamer.epicmegagames.api;

import gg.astromc.slimeloader.loader.SlimeLoader;
import gg.astromc.slimeloader.source.FileSlimeSource;
import gg.astromc.slimeloader.source.SlimeSource;
import net.minestom.server.instance.AnvilLoader;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.Instance;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.nio.file.Path;

public enum ChunkLoaderType {
    ANVIL((instance, path, readOnly) -> new AnvilLoader(path)),
    SLIME((instance, path, readOnly) -> {
        File file = path.toFile();
        if (!file.exists()) {
            return null;
        }
        SlimeSource slimeSource = new FileSlimeSource(file);
        return new SlimeLoader(instance, slimeSource, readOnly);
    });

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
