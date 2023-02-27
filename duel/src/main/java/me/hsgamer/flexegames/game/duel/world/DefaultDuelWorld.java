package me.hsgamer.flexegames.game.duel.world;

import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.instance.block.Block;

import java.util.List;
import java.util.Map;

public class DefaultDuelWorld extends AbstractDuelWorld {
    private final int maxHeight;
    private final List<Pos> pos;
    private final Pos joinPos;

    public DefaultDuelWorld(Map<String, Object> config) {
        super(config);
        this.maxHeight = getMaxHeight();
        this.pos = List.of(
                new Pos(-2, maxHeight, 0, -90, 0),
                new Pos(2, maxHeight, 0, 90, 0),
                new Pos(0, maxHeight, -2, 0, 0),
                new Pos(0, maxHeight, 2, 180, 0)
        );
        this.joinPos = new Pos(0, maxHeight, 0);
    }

    private int getMaxHeight() {
        try {
            return Integer.parseInt(String.valueOf(map.get("max-height")));
        } catch (Exception e) {
            return 2;
        }
    }

    @Override
    protected void modifyInstance(InstanceContainer instance, Arena arena) {
        instance.setGenerator(unit -> {
            unit.modifier().fillHeight(0, 1, Block.BEDROCK);
            if (maxHeight > 1) {
                unit.modifier().fillHeight(1, maxHeight, Block.GRASS_BLOCK);
            }
        });
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
