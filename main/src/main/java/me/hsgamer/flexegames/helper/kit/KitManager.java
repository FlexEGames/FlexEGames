package me.hsgamer.flexegames.helper.kit;

import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.feature.arena.KitFeature;
import me.hsgamer.flexegames.util.MapUtil;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringLinkedMap;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringMap;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.Arena;
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
    private final Map<String, Kit> kitMap;

    public KitManager() {
        this.kitMap = new CaseInsensitiveStringLinkedMap<>();
    }

    public void loadFromConfig(Config config, boolean setup) {
        if (setup) {
            config.setup();
        }
        config.getNormalizedValues(false).forEach((key, value) -> {
            if (value instanceof Map<?, ?> rawMap) {
                Map<String, Object> map = new CaseInsensitiveStringMap<>(MapUtil.toStringObjectMap(rawMap));
                kitMap.put(key, new SimpleKit(map));
            }
        });
    }

    public void addKit(String name, Kit kit) {
        kitMap.put(name, kit);
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

    public KitFeature createFeature(Arena arena, PropertyKeyValue<String> kitProperty) {
        return new KitFeature(arena, this, kitProperty);
    }
}
