package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.converter.ComponentConverter;
import me.hsgamer.flexegames.config.converter.ComponentListConverter;
import me.hsgamer.flexegames.config.converter.StringObjectMapConverter;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.Material;

import java.util.List;
import java.util.Map;

public interface GameDescriptionConfig {
    @ConfigPath(value = "display-name", converter = ComponentConverter.class)
    default Component getDisplayName() {
        return Component.text("Game");
    }

    @ConfigPath(value = "description", converter = ComponentListConverter.class)
    default List<Component> getDescription() {
        return List.of(Component.text("Description"));
    }

    @ConfigPath(value = "display-item", converter = StringObjectMapConverter.class)
    default Map<String, Object> getDisplayItem() {
        return Map.of(
                "material", Material.CHEST.name(),
                "hide", "all"
        );
    }

    @ConfigPath(value = "arena-display-item", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaDisplayItem() {
        return Map.of(
                "material", Material.CHEST.name(),
                "name", "%game%",
                "lore", List.of(
                        "&ePlayers: &f%players%/%max-players%",
                        "&eState: &f%state%"
                ),
                "hide", "all"
        );
    }
}
