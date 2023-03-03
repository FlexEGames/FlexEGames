package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.util.ComponentUtil;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.Material;

import java.util.Map;

public interface DuelMainConfig {
    @ConfigPath("waiting-time")
    default int getWaitingTime() {
        return 60;
    }

    @ConfigPath("ending-time")
    default int getEndingTime() {
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
