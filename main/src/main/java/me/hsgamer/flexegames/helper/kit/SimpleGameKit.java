package me.hsgamer.flexegames.helper.kit;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class SimpleGameKit implements GameKit {
    private final Component displayName;
    private final ItemStack displayItem;
    private final Map<Integer, ItemStack> items;

    public SimpleGameKit(Map<String, Object> map) {
        this.displayName = ComponentConverter.fromString(String.valueOf(map.get("display-name")));
        this.displayItem = ItemUtil.getItemOrStone(map.get("display-item"));

        Object itemsObject = map.get("items");
        if (itemsObject instanceof Map<?, ?> rawMap) {
            Map<Integer, ItemStack> itemStackMap = new HashMap<>();
            rawMap.forEach((key, value) -> {
                try {
                    itemStackMap.put(Integer.parseInt(String.valueOf(key)), ItemUtil.getItemOrStone(value));
                } catch (Exception e) {
                    // IGNORED
                }
            });
            this.items = itemStackMap;
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
