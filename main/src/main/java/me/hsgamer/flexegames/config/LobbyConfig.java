package me.hsgamer.flexegames.config;

import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.Material;

import java.util.*;

/**
 * The {@link me.hsgamer.flexegames.lobby.Lobby} config
 */
public interface LobbyConfig {
    @ConfigPath("world.id")
    default UUID getWorldId() {
        return UUID.randomUUID();
    }

    @ConfigPath(value = "world.type")
    default String getWorldType() {
        return "anvil";
    }

    @ConfigPath("world.name")
    default String getWorldName() {
        return "lobby";
    }

    @ConfigPath("world.spawn-pos")
    default Pos getWorldSpawnPos() {
        return new Pos(0, 0, 0);
    }

    @ConfigPath("world.modifiers")
    default List<Map<String, Object>> getWorldModifiers() {
        return Collections.emptyList();
    }

    @ConfigPath("chat-format")
    default Component getChatFormat() {
        return Component.empty()
                .append(Component.text("%player%").color(NamedTextColor.WHITE))
                .append(Component.text(": ").color(NamedTextColor.YELLOW))
                .append(Component.text("%message%").color(NamedTextColor.GOLD));
    }

    @ConfigPath("board.title")
    default Component getBoardTitle() {
        return Component.text("Lobby").decorate(TextDecoration.BOLD).color(NamedTextColor.YELLOW);
    }

    @ConfigPath("board.lines")
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

    @ConfigPath("inventory.game.title")
    default Component getGameInventoryTitle() {
        return Component.text("Game").decorate(TextDecoration.BOLD).color(NamedTextColor.DARK_RED);
    }

    @ConfigPath("inventory.game.next-page")
    default Map<String, Object> getGameInventoryNextPage() {
        return Map.of(
                "material", Material.GREEN_STAINED_GLASS_PANE.name(),
                "name", "&aNext page"
        );
    }

    @ConfigPath("inventory.game.previous-page")
    default Map<String, Object> getGameInventoryPreviousPage() {
        return Map.of(
                "material", Material.RED_STAINED_GLASS_PANE.name(),
                "name", "&cPrevious page"
        );
    }

    @ConfigPath("inventory.game.arena")
    default Map<String, Object> getGameInventoryArena() {
        return Map.of(
                "material", Material.CHEST.name(),
                "name", "&a&lPlay arena"
        );
    }

    @ConfigPath("inventory.arena.title")
    default Component getArenaInventoryTitle() {
        return Component.text("Arena").decorate(TextDecoration.BOLD).color(NamedTextColor.DARK_RED);
    }

    @ConfigPath("inventory.arena.next-page")
    default Map<String, Object> getArenaInventoryNextPage() {
        return Map.of(
                "material", Material.GREEN_STAINED_GLASS_PANE.name(),
                "name", "&aNext page"
        );
    }

    @ConfigPath("inventory.arena.previous-page")
    default Map<String, Object> getArenaInventoryPreviousPage() {
        return Map.of(
                "material", Material.RED_STAINED_GLASS_PANE.name(),
                "name", "&cPrevious page"
        );
    }

    @ConfigPath("inventory.arena.game")
    default Map<String, Object> getArenaInventoryGame() {
        return Map.of(
                "material", Material.SPRUCE_SIGN.name(),
                "name", "&a&lCreate game"
        );
    }

    @ConfigPath("inventory.arena.global-arena")
    default Map<String, Object> getArenaInventoryGlobalArena() {
        return Map.of(
                "material", Material.CHEST.name(),
                "name", "&e&lGlobal arena"
        );
    }

    @ConfigPath("inventory.arena.my-arena")
    default Map<String, Object> getArenaInventoryMyArena() {
        return Map.of(
                "material", Material.ENDER_CHEST.name(),
                "name", "&e&lMy arena"
        );
    }

    @ConfigPath("hotbar.game")
    default Map<String, Object> getGameHotbar() {
        return Map.of(
                "material", Material.ACACIA_SIGN.name(),
                "name", "&a&lGame",
                "slot", 2
        );
    }

    @ConfigPath("hotbar.arena")
    default Map<String, Object> getArenaHotbar() {
        return Map.of(
                "material", Material.COMPASS.name(),
                "name", "&e&lArena",
                "slot", 4
        );
    }

    @ConfigPath("hotbar.toggle-player")
    default Map<String, Object> getTogglePlayerHotbar() {
        return Map.of(
                "material", Material.ENDER_EYE.name(),
                "name", "&b&lToggle player",
                "slot", 6
        );
    }

    @ConfigPath("hotbar.server-hub")
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
