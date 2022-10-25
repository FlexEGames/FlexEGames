package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.converter.*;
import me.hsgamer.flexegames.util.ChunkLoaderType;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.Material;

import java.util.*;

public interface LobbyConfig {
    @ConfigPath(value = "world.id", converter = UuidConverter.class)
    default UUID getWorldId() {
        return UUID.randomUUID();
    }

    @ConfigPath(value = "world.type", converter = ChunkLoaderTypeConverter.class)
    default ChunkLoaderType getWorldType() {
        return ChunkLoaderType.ANVIL;
    }

    @ConfigPath("world.name")
    default String getWorldName() {
        return "lobby";
    }

    @ConfigPath(value = "world.spawn-pos", converter = PosConverter.class)
    default Pos getWorldSpawnPos() {
        return new Pos(0, 0, 0);
    }

    @ConfigPath(value = "world.modifiers", converter = MapListConverter.class)
    default List<Map<String, Object>> getWorldModifiers() {
        return Collections.emptyList();
    }

    @ConfigPath(value = "chat-format", converter = ComponentConverter.class)
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }

    @ConfigPath(value = "board.title", converter = ComponentConverter.class)
    default Component getBoardTitle() {
        return Component.text("Lobby").decorate(TextDecoration.BOLD).color(NamedTextColor.YELLOW);
    }

    @ConfigPath(value = "board.lines", converter = ComponentListConverter.class)
    default List<Component> getBoardLines() {
        return Arrays.asList(
                Component.text("Welcome to the lobby!").color(NamedTextColor.WHITE),
                Component.text("You can play games here!").color(NamedTextColor.WHITE)
        );
    }

    @ConfigPath("board.update-interval")
    default int getBoardUpdateInterval() {
        return 20;
    }

    @ConfigPath("board.async")
    default boolean isBoardAsync() {
        return true;
    }

    @ConfigPath(value = "inventory.game.title", converter = ComponentConverter.class)
    default Component getGameInventoryTitle() {
        return Component.text("Game").decorate(TextDecoration.BOLD).color(NamedTextColor.DARK_RED);
    }

    @ConfigPath(value = "inventory.game.next-page", converter = StringObjectMapConverter.class)
    default Map<String, Object> getGameInventoryNextPage() {
        return Map.of(
                "material", Material.GREEN_STAINED_GLASS_PANE.name(),
                "name", "&aNext page"
        );
    }

    @ConfigPath(value = "inventory.game.previous-page", converter = StringObjectMapConverter.class)
    default Map<String, Object> getGameInventoryPreviousPage() {
        return Map.of(
                "material", Material.RED_STAINED_GLASS_PANE.name(),
                "name", "&cPrevious page"
        );
    }

    @ConfigPath(value = "inventory.game.arena", converter = StringObjectMapConverter.class)
    default Map<String, Object> getGameInventoryArena() {
        return Map.of(
                "material", Material.CHEST.name(),
                "name", "&a&lPlay arena"
        );
    }

    @ConfigPath(value = "inventory.arena.title", converter = ComponentConverter.class)
    default Component getArenaInventoryTitle() {
        return Component.text("Arena").decorate(TextDecoration.BOLD).color(NamedTextColor.DARK_RED);
    }

    @ConfigPath(value = "inventory.arena.next-page", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaInventoryNextPage() {
        return Map.of(
                "material", Material.GREEN_STAINED_GLASS_PANE.name(),
                "name", "&aNext page"
        );
    }

    @ConfigPath(value = "inventory.arena.previous-page", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaInventoryPreviousPage() {
        return Map.of(
                "material", Material.RED_STAINED_GLASS_PANE.name(),
                "name", "&cPrevious page"
        );
    }

    @ConfigPath(value = "inventory.arena.game", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaInventoryGame() {
        return Map.of(
                "material", Material.SPRUCE_SIGN.name(),
                "name", "&a&lCreate game"
        );
    }

    @ConfigPath(value = "inventory.arena.global-arena", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaInventoryGlobalArena() {
        return Map.of(
                "material", Material.CHEST.name(),
                "name", "&e&lGlobal arena"
        );
    }

    @ConfigPath(value = "inventory.arena.my-arena", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaInventoryMyArena() {
        return Map.of(
                "material", Material.ENDER_CHEST.name(),
                "name", "&e&lMy arena"
        );
    }

    @ConfigPath(value = "hotbar.game", converter = StringObjectMapConverter.class)
    default Map<String, Object> getGameHotbar() {
        return Map.of(
                "material", Material.ACACIA_SIGN.name(),
                "name", "&a&lGame",
                "slot", 2
        );
    }

    @ConfigPath(value = "hotbar.arena", converter = StringObjectMapConverter.class)
    default Map<String, Object> getArenaHotbar() {
        return Map.of(
                "material", Material.COMPASS.name(),
                "name", "&e&lArena",
                "slot", 4
        );
    }

    @ConfigPath(value = "hotbar.toggle-player", converter = StringObjectMapConverter.class)
    default Map<String, Object> getTogglePlayerHotbar() {
        return Map.of(
                "material", Material.ENDER_EYE.name(),
                "name", "&b&lToggle player",
                "slot", 6
        );
    }

    @ConfigPath(value = "hotbar.server-hub", converter = StringObjectMapConverter.class)
    default Map<String, Object> getServerHubHotbar() {
        return Map.of(
                "enable", false,
                "material", Material.REDSTONE_BLOCK.name(),
                "name", "&c&lBack To Hub",
                "slot", 8,
                "server", "hub"
        );
    }
}
