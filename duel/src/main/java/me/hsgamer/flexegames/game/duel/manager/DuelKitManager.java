package me.hsgamer.flexegames.game.duel.manager;

import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.kit.DuelKit;
import me.hsgamer.flexegames.game.duel.kit.SimpleDuelKit;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;
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

public class DuelKitManager {
    private static final DuelKit DEFAULT_DUEL_KIT = new DuelKit() {
        @Override
        public Component getDisplayName() {
            return Component.text("Default").decorate(TextDecoration.BOLD);
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
    private final Map<String, DuelKit> duelKitMap;

    public DuelKitManager(DuelExtension duelExtension) {
        this.config = ConfigGeneratorUtil.createConfig(duelExtension.getDataDirectory().resolve("kit.yml").toFile());
        this.duelKitMap = new CaseInsensitiveStringHashMap<>();
    }

    public void init() {
        config.getNormalizedValues(false).forEach((key, value) -> {
            if (value instanceof Map<?, ?> rawMap) {
                Map<String, Object> map = new CaseInsensitiveStringMap<>(MapUtil.toStringObjectMap(rawMap));
                duelKitMap.put(key, new SimpleDuelKit(map));
            }
        });
    }

    public void clear() {
        duelKitMap.clear();
    }

    public Map<String, DuelKit> getDuelKitMap() {
        return duelKitMap;
    }

    public DuelKit getDuelKit(String name) {
        return duelKitMap.getOrDefault(name, DEFAULT_DUEL_KIT);
    }
}
