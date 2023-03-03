package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.util.ComponentUtil;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.Material;

import java.util.Map;

public interface PveMainConfig {
    @ConfigPath("max-players")
    default int getMaxPlayers() {
        return 10;
    }

    @ConfigPath("min-players")
    default int getMinPlayers() {
        return 1;
    }

    @ConfigPath("waiting-time")
    default int getWaitingTime() {
        return 60;
    }

    @ConfigPath("resting-time")
    default int getRestingTime() {
        return 30;
    }

    @ConfigPath("ending-time")
    default int getEndingTime() {
        return 10;
    }

    @ConfigPath("spawn-delay")
    default int getSpawnDelay() {
        return 500;
    }

    @ConfigPath("mob-per-spawn")
    default int getMobPerSpawn() {
        return 5;
    }

    @ConfigPath("default-kit")
    default Map<String, Object> getDefaultKit() {
        Component displayName = Component.text("Empty Kit").decorate(TextDecoration.BOLD);
        return Map.of(
                "display-name", ComponentUtil.toString(displayName),
                "display-item", Map.of(
                        "material", Material.STONE.name(),
                        "display-name", ComponentUtil.toString(displayName)
                )
        );
    }
}
