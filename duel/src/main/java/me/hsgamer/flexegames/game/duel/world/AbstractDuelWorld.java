package me.hsgamer.flexegames.game.duel.world;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.util.FullBrightDimension;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.instance.Instance;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.item.ItemStack;

import java.util.Map;

public abstract class AbstractDuelWorld implements DuelWorld {
    protected final Map<String, Object> map;
    private final Component displayName;
    private final ItemStack displayItem;

    protected AbstractDuelWorld(Map<String, Object> map) {
        this.map = map;
        this.displayName = ComponentConverter.fromString(String.valueOf(map.get("display-name")));
        this.displayItem = ItemUtil.getItemOrStone(map.get("display-item"));
    }

    protected abstract void modifyInstance(InstanceContainer instance, Arena arena);

    protected double getBorderDiameter() {
        try {
            return Double.parseDouble(String.valueOf(map.get("border-diameter")));
        } catch (Exception e) {
            return 50.0;
        }
    }

    @Override
    public final Instance createInstance(Arena arena) {
        InstanceContainer instance = MinecraftServer.getInstanceManager().createInstanceContainer(FullBrightDimension.INSTANCE);
        modifyInstance(instance, arena);
        Pos joinPos = getJoinPos();
        instance.setTimeRate(0);
        instance.setTime(6000);
        instance.getWorldBorder().setCenter((float) joinPos.x(), (float) joinPos.z());
        instance.getWorldBorder().setDiameter(getBorderDiameter());
        return instance;
    }

    @Override
    public Component getDisplayName() {
        return displayName;
    }

    @Override
    public ItemStack getDisplayItem() {
        return displayItem;
    }
}
