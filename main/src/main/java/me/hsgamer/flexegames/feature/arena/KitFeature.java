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

/**
 * The feature for {@link Kit}.
 * It will get the kit from the {@link KitManager} with the {@link PropertyKeyValue}.
 */
public class KitFeature implements Feature {
    private final Arena arena;
    private final KitManager kitManager;
    private final PropertyKeyValue<String> kitProperty;
    private @Getter Kit kit;

    /**
     * Create a new feature
     *
     * @param arena       the arena
     * @param kitManager  the kit manager
     * @param kitProperty the kit property
     */
    public KitFeature(Arena arena, KitManager kitManager, PropertyKeyValue<String> kitProperty) {
        this.arena = arena;
        this.kitManager = kitManager;
        this.kitProperty = kitProperty;
    }

    @Override
    public void init() {
        this.kit = kitManager.getKit(arena.getFeature(GameFeature.class).propertyMap().getProperty(kitProperty));
    }

    /**
     * Give the kit to the player
     *
     * @param player       the player
     * @param itemModifier additional modifier for the item
     */
    public void giveKit(Player player, UnaryOperator<ItemStack> itemModifier) {
        var inventory = player.getInventory();
        kit.getItems().forEach((slot, item) -> {
            if (slot < 0 || slot >= inventory.getSize()) return;
            item = itemModifier.apply(item);
            player.getInventory().setItemStack(slot, item);
        });
    }

    /**
     * Give the kit to all players in the instance
     *
     * @param instance     the instance
     * @param itemModifier additional modifier for the item
     */
    public void giveKit(Instance instance, UnaryOperator<ItemStack> itemModifier) {
        instance.getPlayers().forEach(player -> giveKit(player, itemModifier));
    }

    /**
     * Give the kit to the player
     *
     * @param player the player
     */
    public void giveKit(Player player) {
        giveKit(player, UnaryOperator.identity());
    }

    /**
     * Give the kit to all players in the instance
     *
     * @param instance the instance
     */
    public void giveKit(Instance instance) {
        giveKit(instance, UnaryOperator.identity());
    }
}
