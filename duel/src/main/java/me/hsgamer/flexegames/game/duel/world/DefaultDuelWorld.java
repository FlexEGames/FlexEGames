package me.hsgamer.flexegames.game.duel.world;

import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.instance.block.Block;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.List;
import java.util.Map;

public class DefaultDuelWorld extends AbstractDuelWorld {
    private final int maxHeight;
    private final List<Pos> pos;
    private final Pos joinPos;

    private DefaultDuelWorld(Component displayName, ItemStack displayItem, double borderDiameter, int maxHeight) {
        super(displayName, displayItem, borderDiameter);
        this.maxHeight = maxHeight;
        this.pos = List.of(
                new Pos(-2, maxHeight, 0, -90, 0),
                new Pos(2, maxHeight, 0, 90, 0),
                new Pos(0, maxHeight, -2, 0, 0),
                new Pos(0, maxHeight, 2, 180, 0)
        );
        this.joinPos = new Pos(0, maxHeight, 0);
    }

    public DefaultDuelWorld(Map<String, Object> map) {
        super(map);
        this.maxHeight = getMaxHeight(map);
        this.pos = List.of(
                new Pos(-2, maxHeight, 0, -90, 0),
                new Pos(2, maxHeight, 0, 90, 0),
                new Pos(0, maxHeight, -2, 0, 0),
                new Pos(0, maxHeight, 2, 180, 0)
        );
        this.joinPos = new Pos(0, maxHeight, 0);
    }

    public static DefaultDuelWorld defaultDuelWorld() {
        Component name = Component.text("Default World").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD);
        return new DefaultDuelWorld(
                name,
                ItemUtil.stripItalics(ItemStack.of(Material.GRASS_BLOCK).withDisplayName(name)),
                50,
                2
        );
    }

    private static int getMaxHeight(Map<String, Object> map) {
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
