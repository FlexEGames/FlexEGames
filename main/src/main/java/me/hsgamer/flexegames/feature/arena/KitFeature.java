package me.hsgamer.flexegames.feature.arena;

import lombok.Getter;
import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.helper.kit.Kit;
import me.hsgamer.flexegames.helper.kit.KitManager;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;
import net.minestom.server.instance.Instance;
import net.minestom.server.item.ItemStack;

import java.util.function.UnaryOperator;

public class KitFeature implements Feature {
    private final Arena arena;
    private final KitManager kitManager;
    private final PropertyKeyValue<String> kitProperty;
    private @Getter Kit kit;

    public KitFeature(Arena arena, KitManager kitManager, PropertyKeyValue<String> kitProperty) {
        this.arena = arena;
        this.kitManager = kitManager;
        this.kitProperty = kitProperty;
    }

    @Override
    public void init() {
        this.kit = kitManager.getKit(arena.getFeature(GameFeature.class).propertyMap().getProperty(kitProperty));
    }

    public void giveKit(Player player, UnaryOperator<ItemStack> itemModifier) {
        var inventory = player.getInventory();
        kit.getItems().forEach((slot, item) -> {
            if (slot < 0 || slot >= inventory.getSize()) return;
            item = itemModifier.apply(item);
            player.getInventory().setItemStack(slot, item);
        });
    }

    public void giveKit(Instance instance, UnaryOperator<ItemStack> itemModifier) {
        instance.getPlayers().forEach(player -> giveKit(player, itemModifier));
    }

    public void giveKit(Player player) {
        giveKit(player, UnaryOperator.identity());
    }

    public void giveKit(Instance instance) {
        giveKit(instance, UnaryOperator.identity());
    }
}
