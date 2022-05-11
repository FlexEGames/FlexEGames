package me.hsgamer.flexegames.util;

import net.minestom.server.instance.Chunk;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.Instance;
import net.minestom.server.instance.block.Block;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiFunction;

public class InstanceChunkLoader implements IChunkLoader {
    private final IChunkLoader loader;
    private final BiFunction<Chunk, Block, Block> blockModifier;
    private final boolean ignoreAir;

    public InstanceChunkLoader(IChunkLoader loader, BiFunction<Chunk, Block, Block> blockModifier, boolean ignoreAir) {
        this.loader = loader;
        this.blockModifier = blockModifier;
        this.ignoreAir = ignoreAir;
    }

    public InstanceChunkLoader(IChunkLoader loader, BiFunction<Chunk, Block, Block> blockModifier) {
        this(loader, blockModifier, true);
    }

    @Override
    public @NotNull CompletableFuture<@Nullable Chunk> loadChunk(@NotNull Instance instance, int chunkX, int chunkZ) {
        return loader.loadChunk(instance, chunkX, chunkZ).thenApplyAsync(chunk -> {
            if (chunk == null) {
                return null;
            }
            for (int y = instance.getDimensionType().getMinY(); y < instance.getDimensionType().getMaxY(); y++) {
                for (int x = 0; x < 16; x++) {
                    for (int z = 0; z < 16; z++) {
                        Block block = chunk.getBlock(x, y, z);
                        if (!block.isAir() || !ignoreAir) {
                            block = blockModifier.apply(chunk, block);
                            chunk.setBlock(x, y, z, block);
                        }
                    }
                }
            }
            return chunk;
        });
    }

    @Override
    public @NotNull CompletableFuture<Void> saveChunk(@NotNull Chunk chunk) {
        return loader.saveChunk(chunk);
    }

    @Override
    public void loadInstance(@NotNull Instance instance) {
        IChunkLoader.super.loadInstance(instance);
    }

    @Override
    public @NotNull CompletableFuture<Void> saveInstance(@NotNull Instance instance) {
        return loader.saveInstance(instance);
    }

    @Override
    public @NotNull CompletableFuture<Void> saveChunks(@NotNull Collection<Chunk> chunks) {
        return loader.saveChunks(chunks);
    }

    @Override
    public boolean supportsParallelSaving() {
        return loader.supportsParallelSaving();
    }

    @Override
    public boolean supportsParallelLoading() {
        return loader.supportsParallelLoading();
    }
}
