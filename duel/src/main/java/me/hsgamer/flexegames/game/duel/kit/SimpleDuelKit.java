package me.hsgamer.flexegames.game.duel.kit;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class SimpleDuelKit implements DuelKit {
    private final Component displayName;
    private final ItemStack displayItem;
    private final Map<Integer, ItemStack> items;

    public SimpleDuelKit(Map<String, Object> map) {
        this.displayName = ComponentConverter.fromString(String.valueOf(map.get("display-name")));
        this.displayItem = ItemUtil.getItemOrStone(map.get("display-item"));

        Object itemsObject = map.get("items");
        if (itemsObject instanceof Map<?, ?> rawMap) {
            Map<Integer, ItemStack> items = new HashMap<>();
            rawMap.forEach((key, value) -> {
                try {
                    items.put(Integer.parseInt(String.valueOf(key)), ItemUtil.getItemOrStone(value));
                } catch (Exception e) {
                    // IGNORED
                }
            });
            this.items = items;
        } else {
            this.items = Collections.emptyMap();
        }
    }

    @Override
    public Component getDisplayName() {
        return displayName;
    }

    @Override
    public ItemStack getDisplayItem() {
        return displayItem;
    }

    @Override
    public Map<Integer, ItemStack> getItems() {
        return items;
    }
}
