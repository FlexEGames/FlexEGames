package me.hsgamer.flexegames.util.kit;

import me.hsgamer.flexegames.util.MapUtil;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringHashMap;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringMap;
import me.hsgamer.hscore.config.Config;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Collections;
import java.util.Map;

public class GameKitManager {
    private static final GameKit EMPTY_KIT = new GameKit() {
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
    private final Map<String, GameKit> gameKitMap;

    public GameKitManager(Config config) {
        this.config = config;
        this.gameKitMap = new CaseInsensitiveStringHashMap<>();
    }

    public void init() {
        config.getNormalizedValues(false).forEach((key, value) -> {
            if (value instanceof Map<?, ?> rawMap) {
                Map<String, Object> map = new CaseInsensitiveStringMap<>(MapUtil.toStringObjectMap(rawMap));
                gameKitMap.put(key, new SimpleGameKit(map));
            }
        });
    }

    public void clear() {
        gameKitMap.clear();
    }

    public Map<String, GameKit> getGameKitMap() {
        return gameKitMap;
    }

    public GameKit getGameKit(String name) {
        return gameKitMap.getOrDefault(name, EMPTY_KIT);
    }
}
