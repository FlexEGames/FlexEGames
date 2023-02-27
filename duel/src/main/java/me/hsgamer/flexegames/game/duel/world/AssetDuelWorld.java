package me.hsgamer.flexegames.game.duel.world;

import me.hsgamer.flexegames.builder.ChunkLoaderBuilder;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.InstanceContainer;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

public class AssetDuelWorld extends AbstractDuelWorld {
    private final String worldLoader;
    private final Path worldPath;

    public AssetDuelWorld(Map<String, Object> map) {
        super(map);
        this.worldLoader = String.valueOf(map.getOrDefault("world-loader", "anvil"));

        String worldName = String.valueOf(map.getOrDefault("world-name", "duel"));
        File worldFile = AssetUtil.getWorldFile(worldName);
        if (!worldFile.exists()) {
            throw new IllegalStateException("Cannot find the world file");
        }
        this.worldPath = worldFile.toPath();
    }

    @Override
    protected void modifyInstance(InstanceContainer instance, Arena arena) {
        IChunkLoader chunkLoader = ChunkLoaderBuilder.INSTANCE.build(worldLoader, instance, worldPath).orElseThrow(() -> new IllegalStateException("Cannot load the chunk loader"));
        instance.setChunkLoader(chunkLoader);
    }

    @Override
    public List<Pos> getPos() {
        return null;
    }

    @Override
    public Pos getJoinPos() {
        return null;
    }
}
