package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.api.chunk.ChunkLoaderType;
import me.hsgamer.flexegames.config.path.*;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.StickyConfigPath;
import me.hsgamer.hscore.config.path.impl.MapConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.Material;

import java.io.File;
import java.util.*;

public class LobbyConfig extends PathableConfig {
    public static final ConfigPath<UUID> UNIQUE_ID = new UuidPath("unique-id", UUID.randomUUID());
    public static final ConfigPath<ChunkLoaderType> WORLD_TYPE = new ChunkLoaderTypePath("world-type", ChunkLoaderType.ANVIL);
    public static final ConfigPath<String> WORLD_NAME = Paths.stringPath("world-name", "lobby");
    public static final ConfigPath<Pos> POSITION = new PosPath("position", new Pos(0, 0, 0));
    public static final ConfigPath<Component> BOARD_TITLE = new StickyConfigPath<>(new ComponentPath("board.title", Component.text("Lobby").decorate(TextDecoration.BOLD).color(NamedTextColor.YELLOW)));
    public static final ConfigPath<List<Component>> BOARD_LINES = new StickyConfigPath<>(new ComponentListPath("board.lines",Arrays.asList(
            Component.text("Welcome to the lobby!").color(NamedTextColor.WHITE),
            Component.text("You can play games here!").color(NamedTextColor.WHITE)
    )));
    public static final ConfigPath<Integer> BOARD_UPDATE_TIME = Paths.integerPath("board.update-time", 20);
    public static final ConfigPath<Boolean> BOARD_ASYNC = Paths.booleanPath("board.async", true);
    public static final ConfigPath<Component> INVENTORY_TEMPLATE_TITLE = new ComponentPath("inventory.template.title", Component.text("Template").decorate(TextDecoration.BOLD).color(NamedTextColor.DARK_RED));
    public static final MapConfigPath INVENTORY_TEMPLATE_NEXT_PAGE = new MapConfigPath("inventory.template.next-page", Map.of(
            "material", Material.GREEN_STAINED_GLASS_PANE.name(),
            "name", "&aNext page"
    ));
    public static final MapConfigPath INVENTORY_TEMPLATE_PREVIOUS_PAGE = new MapConfigPath("inventory.template.previous-page", Map.of(
            "material", Material.RED_STAINED_GLASS_PANE.name(),
            "name", "&cPrevious page"
    ));
    public static final MapConfigPath INVENTORY_TEMPLATE_ARENA = new MapConfigPath("inventory.template.arena", Map.of(
            "material", Material.CHEST.name(),
            "name", "&aPlay arena"
    ));
    public static final ConfigPath<Component> INVENTORY_ARENA_TITLE = new ComponentPath("inventory.arena.title", Component.text("Arena").decorate(TextDecoration.BOLD).color(NamedTextColor.DARK_RED));
    public static final MapConfigPath INVENTORY_ARENA_NEXT_PAGE = new MapConfigPath("inventory.arena.next-page", Map.of(
            "material", Material.GREEN_STAINED_GLASS_PANE.name(),
            "name", "&aNext page"
    ));
    public static final MapConfigPath INVENTORY_ARENA_PREVIOUS_PAGE = new MapConfigPath("inventory.arena.previous-page", Map.of(
            "material", Material.RED_STAINED_GLASS_PANE.name(),
            "name", "&cPrevious page"
    ));
    public static final MapConfigPath INVENTORY_ARENA_TEMPLATE = new MapConfigPath("inventory.arena.template", Map.of(
            "material", Material.SPRUCE_SIGN.name(),
            "name", "&e&lTemplate"
    ));
    public static final MapConfigPath INVENTORY_ARENA_GLOBAL_ARENA = new MapConfigPath("inventory.arena.global-arena", Map.of(
            "material", Material.CHEST.name(),
            "name", "&e&lGlobal arena"
    ));
    public static final MapConfigPath INVENTORY_ARENA_MY_ARENA = new MapConfigPath("inventory.arena.my-arena", Map.of(
            "material", Material.ENDER_CHEST.name(),
            "name", "&e&lMy arena"
    ));
    public static final MapConfigPath HOTBAR_CREATOR = new MapConfigPath("hotbar.creator", Map.of(
            "material", Material.ACACIA_SIGN.name(),
            "name", "&a&lTemplate",
            "slot", 1
    ));
    public static final MapConfigPath HOTBAR_SELECTOR = new MapConfigPath("hotbar.selector", Map.of(
            "material", Material.COMPASS.name(),
            "name", "&e&lArena",
            "slot", 4
    ));
    public static final MapConfigPath HOTBAR_TOGGLE_PLAYER = new MapConfigPath("hotbar.toggle-player", Map.of(
            "material", Material.ENDER_EYE.name(),
            "name", "&c&lToggle player",
            "slot", 7
    ));
    public static final MapListPath MODIFIERS = new MapListPath("modifiers", Collections.emptyList());

    public LobbyConfig() {
        super(new YamlProvider().loadConfiguration(new File("lobby.yml")));
    }
}
