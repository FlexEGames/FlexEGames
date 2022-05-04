package me.hsgamer.epicmegagames.config;

import me.hsgamer.epicmegagames.config.path.LegacyComponentListPath;
import me.hsgamer.epicmegagames.config.path.LegacyComponentPath;
import me.hsgamer.epicmegagames.config.path.LoaderTypePath;
import me.hsgamer.epicmegagames.config.path.PosPath;
import me.hsgamer.epicmegagames.util.LoaderType;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class LobbyConfig extends PathableConfig {
    public static final ConfigPath<LoaderType> WORLD_TYPE = new LoaderTypePath("world-type", LoaderType.ANVIL);
    public static final ConfigPath<String> WORLD_NAME = Paths.stringPath("world-name", "lobby");
    public static final ConfigPath<Pos> POSITION = new PosPath("position", new Pos(0, 0, 0));
    public static final ConfigPath<Component> BOARD_TITLE = new LegacyComponentPath("board.title", Component.text("Lobby").decorate(TextDecoration.BOLD).color(NamedTextColor.YELLOW));
    public static final ConfigPath<List<Component>> BOARD_LINES = new LegacyComponentListPath("board.lines", Arrays.asList(
            Component.text("Welcome to the lobby!").color(NamedTextColor.WHITE),
            Component.text("You can play games here!").color(NamedTextColor.WHITE)
    ));
    public static final ConfigPath<Integer> BOARD_UPDATE_TIME = Paths.integerPath("board.update-time", 20);

    public LobbyConfig() {
        super(new YamlProvider().loadConfiguration(new File("lobby.yml")));
    }
}
