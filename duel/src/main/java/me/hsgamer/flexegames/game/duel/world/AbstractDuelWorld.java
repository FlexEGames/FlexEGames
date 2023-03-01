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
    private final Component displayName;
    private final ItemStack displayItem;
    private final double borderDiameter;

    protected AbstractDuelWorld(Component displayName, ItemStack displayItem, double borderDiameter) {
        this.displayName = displayName;
        this.displayItem = displayItem;
        this.borderDiameter = borderDiameter;
    }

    protected AbstractDuelWorld(Map<String, Object> map) {
        this(ComponentConverter.fromString(String.valueOf(map.get("display-name"))), ItemUtil.getItemOrStone(map.get("display-item")), getBorderDiameter(map));
    }

    private static double getBorderDiameter(Map<String, Object> map) {
        try {
            return Double.parseDouble(String.valueOf(map.get("border-diameter")));
        } catch (Exception e) {
            return 50.0;
        }
    }

    protected abstract void modifyInstance(InstanceContainer instance, Arena arena);

    @Override
    public final Instance createInstance(Arena arena) {
        InstanceContainer instance = MinecraftServer.getInstanceManager().createInstanceContainer(FullBrightDimension.INSTANCE);
        modifyInstance(instance, arena);
        Pos joinPos = getJoinPos();
        instance.setTimeRate(0);
        instance.setTime(6000);
        instance.getWorldBorder().setCenter((float) joinPos.x(), (float) joinPos.z());
        instance.getWorldBorder().setDiameter(borderDiameter);
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
