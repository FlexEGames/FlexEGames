package me.hsgamer.flexegames.helper.kit;

import me.hsgamer.flexegames.util.MapUtil;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringLinkedMap;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringMap;
import me.hsgamer.hscore.config.Config;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Collections;
import java.util.Map;

public class KitManager {
    private static final Kit EMPTY_KIT = new Kit() {
        @Override
        public Component getDisplayName() {
            return Component.text("Empty Kit").decorate(TextDecoration.BOLD);
        }

        @Override
        public ItemStack getDisplayItem() {
            return ItemStack.of(Material.STONE).withDisplayName(getDisplayName());
        }

        @Override
        public Map<Integer, ItemStack> getItems() {
            return Collections.emptyMap();
        }
    };
    private final Config config;
    private final Map<String, Kit> kitMap;

    public KitManager(Config config, boolean setup) {
        this.config = config;
        if (setup) {
            config.setup();
        }
        this.kitMap = new CaseInsensitiveStringLinkedMap<>();
    }

    public void init() {
        config.getNormalizedValues(false).forEach((key, value) -> {
            if (value instanceof Map<?, ?> rawMap) {
                Map<String, Object> map = new CaseInsensitiveStringMap<>(MapUtil.toStringObjectMap(rawMap));
                kitMap.put(key, new SimpleKit(map));
            }
        });
    }

    public void clear() {
        kitMap.clear();
    }

    public Map<String, Kit> getKitMap() {
        return kitMap;
    }

    public Kit getKit(String name) {
        return kitMap.getOrDefault(name, EMPTY_KIT);
    }
}
