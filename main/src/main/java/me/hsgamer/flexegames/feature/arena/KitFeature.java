package me.hsgamer.flexegames.feature.arena;

import lombok.Getter;
import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.helper.kit.Kit;
import me.hsgamer.flexegames.helper.kit.KitManager;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;
import net.minestom.server.instance.Instance;
import net.minestom.server.inventory.TransactionOption;
import net.minestom.server.item.ItemStack;
import net.minestom.server.tag.Tag;

public class KitFeature implements Feature {
    private final Arena arena;
    private final KitManager kitManager;
    private final PropertyKeyValue<String> kitProperty;
    private final Tag<Boolean> kitTag = Tag.Boolean("games:kit").defaultValue(false);
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

    public void giveKit(Player player) {
        var inventory = player.getInventory();
        kit.getItems().forEach((slot, item) -> {
            if (slot < 0 || slot >= inventory.getSize()) return;
            var taggedItem = item.withTag(kitTag, true);
            player.getInventory().setItemStack(slot, taggedItem);
        });
    }

    public void giveKit(Instance instance) {
        instance.getPlayers().forEach(this::giveKit);
    }

    public void clearKit(Player player) {
        var inventory = player.getInventory();
        for (ItemStack item : inventory.getItemStacks()) {
            if (Boolean.TRUE.equals(item.getTag(kitTag)))
                player.getInventory().takeItemStack(item, TransactionOption.ALL);
        }
    }

    public void clearKit(Instance instance) {
        instance.getPlayers().forEach(this::clearKit);
    }
}
