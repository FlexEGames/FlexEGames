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

/**
 * The {@link Kit} manager
 */
public class KitManager {
    private final Map<String, Kit> kitMap;
    private Kit defaultKit = new Kit() {
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

    /**
     * Create a new {@link KitManager}
     */
    public KitManager() {
        this.kitMap = new CaseInsensitiveStringLinkedMap<>();
    }

    /**
     * Load the kits from the config
     *
     * @param config the config
     * @param setup  whether to set up the config or not
     */
    public void loadFromConfig(Config config, boolean setup) {
        if (setup) {
            config.setup();
        }
        config.getNormalizedValues(false).forEach((key, value) -> {
            if (value instanceof Map<?, ?> rawMap) {
                Map<String, Object> map = new CaseInsensitiveStringMap<>(MapUtil.toStringObjectMap(rawMap));
                add(key, new SimpleKit(map));
            }
        });
    }

    /**
     * Add a kit
     *
     * @param name the name of the kit
     * @param kit  the kit
     */
    public void add(String name, Kit kit) {
        kitMap.put(name, kit);
    }

    /**
     * Remove a kit
     *
     * @param name the name of the kit
     */
    public void remove(String name) {
        kitMap.remove(name);
    }

    /**
     * Clear all kits
     */
    public void clear() {
        kitMap.clear();
    }

    /**
     * Get the kit map
     *
     * @return the kit map
     */
    public Map<String, Kit> getKitMap() {
        return Collections.unmodifiableMap(kitMap);
    }

    /**
     * Get the kit
     *
     * @param name the name of the kit
     * @return the kit or the default kit if not found
     */
    public Kit getKit(String name) {
        return kitMap.getOrDefault(name, defaultKit);
    }

    /**
     * Get the default kit
     *
     * @return the default kit
     */
    public Kit getDefaultKit() {
        return defaultKit;
    }

    /**
     * Set the default kit
     *
     * @param defaultKit the default kit
     */
    public void setDefaultKit(Kit defaultKit) {
        this.defaultKit = defaultKit;
    }

    /**
     * Create a new {@link KitFeature}
     *
     * @param arena       the arena
     * @param kitProperty the kit property
     * @return the kit feature
     */
    public KitFeature createFeature(Arena arena, PropertyKeyValue<String> kitProperty) {
        return new KitFeature(arena, this, kitProperty);
    }
}
