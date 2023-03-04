package me.hsgamer.flexegames.helper.kit;

import me.hsgamer.flexegames.config.converter.NumberObjectMapConverter;
import me.hsgamer.flexegames.util.ComponentUtil;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * A simple implementation of {@link Kit}
 */
public class SimpleKit implements Kit {
    private final Component displayName;
    private final ItemStack displayItem;
    private final Map<Integer, ItemStack> items;

    /**
     * Create a new kit
     *
     * @param displayName the display name
     * @param displayItem the display item
     * @param items       the items
     */
    public SimpleKit(Component displayName, ItemStack displayItem, Map<Integer, ItemStack> items) {
        this.displayName = displayName;
        this.displayItem = displayItem;
        this.items = items;
    }

    /**
     * Create a new kit from a map.
     * The map should contain:
     * <ul>
     *     <li>{@code display-name} as {@link String}</li>
     *     <li>{@code display-item} as {@link Map}</li>
     *     <li>{@code items} as {@link Map} of {@link Integer} and {@link ItemStack}</li>
     * </ul>
     *
     * @param map the map
     */
    public SimpleKit(Map<String, Object> map) {
        this.displayName = ComponentUtil.fromString(String.valueOf(map.get("display-name")));
        this.displayItem = ItemUtil.getItemOrStone(map.get("display-item"));

        Object itemsObject = map.get("items");
        Map<Number, Map<String, Object>> rawItemStackMap = new NumberObjectMapConverter().convert(itemsObject);
        if (rawItemStackMap != null) {
            Map<Integer, ItemStack> itemStackMap = new HashMap<>();
            rawItemStackMap.forEach((key, value) -> {
                try {
                    itemStackMap.put(key.intValue(), ItemUtil.getItemOrStone(value));
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
