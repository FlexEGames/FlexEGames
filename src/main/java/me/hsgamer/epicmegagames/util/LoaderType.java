package me.hsgamer.epicmegagames.util;

import gg.astromc.slimeloader.loader.SlimeLoader;
import gg.astromc.slimeloader.source.FileSlimeSource;
import gg.astromc.slimeloader.source.SlimeSource;
import net.minestom.server.instance.AnvilLoader;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.Instance;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.nio.file.Path;
import java.util.function.BiFunction;
import java.util.function.Function;

public enum LoaderType {
    ANVIL(s -> new AnvilLoader(Path.of(s))),
    SLIME((instance, s) -> {
        File file = new File(s + ".slime");
        if (!file.exists()) {
            return null;
        }
        SlimeSource slimeSource = new FileSlimeSource(file);
        return new SlimeLoader(instance, slimeSource, true);
    });

    private final BiFunction<Instance, String, @Nullable IChunkLoader> function;

    LoaderType(BiFunction<Instance, String, @Nullable IChunkLoader> function) {
        this.function = function;
    }

    LoaderType(Function<String, @Nullable IChunkLoader> function) {
        this((i, s) -> function.apply(s));
    }

    @Nullable
    public IChunkLoader getLoader(Instance instance, String name) {
        return function.apply(instance, name);
    }
}
