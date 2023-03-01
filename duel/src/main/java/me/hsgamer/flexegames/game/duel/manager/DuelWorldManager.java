package me.hsgamer.flexegames.game.duel.manager;

import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.world.AssetDuelWorld;
import me.hsgamer.flexegames.game.duel.world.DefaultDuelWorld;
import me.hsgamer.flexegames.game.duel.world.DuelWorld;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;
import me.hsgamer.flexegames.util.MapUtil;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringLinkedMap;
import me.hsgamer.hscore.collections.map.CaseInsensitiveStringMap;
import me.hsgamer.hscore.config.Config;
import net.minestom.server.MinecraftServer;

import java.util.Map;

public class DuelWorldManager {
    private static final DefaultDuelWorld DEFAULT_DUEL_WORLD = DefaultDuelWorld.defaultDuelWorld();
    private final Config config;
    private final Map<String, DuelWorld> duelWorldMap;

    public DuelWorldManager(DuelExtension duelExtension) {
        this.config = ConfigGeneratorUtil.createConfig(duelExtension.getDataDirectory().resolve("world.yml").toFile());
        this.config.setup();
        this.duelWorldMap = new CaseInsensitiveStringLinkedMap<>();
    }

    public void init() {
        config.getNormalizedValues(false).forEach((key, value) -> {
            if (value instanceof Map<?, ?> rawMap) {
                Map<String, Object> map = new CaseInsensitiveStringMap<>(MapUtil.toStringObjectMap(rawMap));
                String type = String.valueOf(map.getOrDefault("type", "default"));
                try {
                    if (type.equalsIgnoreCase("world")) {
                        duelWorldMap.put(key, new AssetDuelWorld(map));
                        return;
                    }
                } catch (Exception e) {
                    MinecraftServer.LOGGER.error("Failed to load world " + key, e);
                }
                duelWorldMap.put(key, new DefaultDuelWorld(map));
            }
        });
    }

    public void clear() {
        duelWorldMap.clear();
    }

    public Map<String, DuelWorld> getDuelWorldMap() {
        return duelWorldMap;
    }

    public DuelWorld getDuelWorld(String name) {
        return duelWorldMap.getOrDefault(name, DEFAULT_DUEL_WORLD);
    }
}
