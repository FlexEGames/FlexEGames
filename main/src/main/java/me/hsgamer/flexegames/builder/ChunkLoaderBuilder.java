package me.hsgamer.flexegames.builder;

import me.hsgamer.hscore.builder.Builder;
import net.minestom.server.instance.AnvilLoader;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.Instance;

import java.nio.file.Path;
import java.util.Optional;

/**
 * The builder for {@link IChunkLoader}
 */
public class ChunkLoaderBuilder extends Builder<ChunkLoaderBuilder.Input, IChunkLoader> {
    /**
     * The singleton instance
     */
    public static final ChunkLoaderBuilder INSTANCE = new ChunkLoaderBuilder();

    private ChunkLoaderBuilder() {
        register(input -> new AnvilLoader(input.path), "anvil");
    }

    /**
     * Build the {@link IChunkLoader} from the input
     *
     * @param type     the type
     * @param instance the instance
     * @param path     the path
     * @param readOnly whether it is read-only
     * @return the {@link IChunkLoader}
     */
    public Optional<IChunkLoader> build(String type, Instance instance, Path path, boolean readOnly) {
        return build(type, new Input(instance, path, readOnly));
    }

    /**
     * Build the {@link IChunkLoader} from the input
     *
     * @param type     the type
     * @param instance the instance
     * @param path     the path
     * @return the {@link IChunkLoader}
     */
    public Optional<IChunkLoader> build(String type, Instance instance, Path path) {
        return build(type, instance, path, false);
    }

    /**
     * Build the {@link IChunkLoader} from the input
     *
     * @param type     the type
     * @param instance the instance
     * @param path     the path
     * @param readOnly whether it is read-only
     * @return the {@link IChunkLoader}
     */
    public Optional<IChunkLoader> build(String type, Instance instance, String path, boolean readOnly) {
        return build(type, instance, Path.of(path), readOnly);
    }

    /**
     * Build the {@link IChunkLoader} from the input
     *
     * @param type     the type
     * @param instance the instance
     * @param path     the path
     * @return the {@link IChunkLoader}
     */
    public Optional<IChunkLoader> build(String type, Instance instance, String path) {
        return build(type, instance, path, false);
    }

    /**
     * The input for {@link ChunkLoaderBuilder}
     *
     * @param instance the instance
     * @param path     the path
     * @param readOnly whether it is read-only
     */
    public record Input(Instance instance, Path path, boolean readOnly) {
    }
}
