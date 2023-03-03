package me.hsgamer.flexegames.game.duel.world;

import me.hsgamer.flexegames.builder.ChunkLoaderBuilder;
import me.hsgamer.flexegames.config.converter.PosListConverter;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.instance.IChunkLoader;
import net.minestom.server.instance.InstanceContainer;

import java.io.File;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class AssetDuelWorld extends AbstractDuelWorld {
    private final String worldLoader;
    private final Path worldPath;
    private final List<Pos> pos;
    private final Pos joinPos;

    public AssetDuelWorld(Map<String, Object> map) {
        super(map);
        this.worldLoader = String.valueOf(map.getOrDefault("world-loader", "anvil"));

        String worldName = String.valueOf(map.getOrDefault("world-name", "duel"));
        File worldFile = AssetUtil.getWorldFile(worldName);
        if (!worldFile.exists()) {
            throw new IllegalStateException("Cannot find the world file");
        }
        this.worldPath = worldFile.toPath();

        this.pos = Optional.ofNullable(map.get("pos-list"))
                .map(o -> new PosListConverter().convert(o))
                .<List<Pos>>map(o -> {
                    if (o instanceof List<?> list) {
                        return list.stream()
                                .filter(Pos.class::isInstance)
                                .map(Pos.class::cast)
                                .toList();
                    } else {
                        return Collections.emptyList();
                    }
                })
                .filter(list -> !list.isEmpty())
                .orElseThrow(() -> new IllegalStateException("Cannot find the pos list"));
        this.joinPos = Optional.ofNullable(map.get("join-pos"))
                .map(o -> new PosListConverter().convert(o))
                .map(o -> {
                    if (o instanceof Pos convertedPos) {
                        return convertedPos;
                    } else {
                        return null;
                    }
                })
                .orElseThrow(() -> new IllegalStateException("Cannot find the join pos"));
    }

    @Override
    protected void modifyInstance(InstanceContainer instance, Arena arena) {
        IChunkLoader chunkLoader = ChunkLoaderBuilder.INSTANCE.build(worldLoader, instance, worldPath).orElseThrow(() -> new IllegalStateException("Cannot load the chunk loader"));
        instance.setChunkLoader(chunkLoader);
    }

    @Override
    public List<Pos> getPos() {
        return pos;
    }

    @Override
    public Pos getJoinPos() {
        return joinPos;
    }
}
