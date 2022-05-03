package me.hsgamer.epicmegagames.config;

import me.hsgamer.epicmegagames.config.path.LoaderTypePath;
import me.hsgamer.epicmegagames.config.path.PosPath;
import me.hsgamer.epicmegagames.util.LoaderType;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.minestom.server.coordinate.Pos;

import java.io.File;

public class LobbyConfig extends PathableConfig {
    public static final ConfigPath<LoaderType> WORLD_TYPE = new LoaderTypePath("world-type", LoaderType.ANVIL);
    public static final ConfigPath<String> WORLD_NAME = Paths.stringPath("world-name", "lobby");
    public static final ConfigPath<Pos> POSITION = new PosPath("position", new Pos(0, 0, 0));

    public LobbyConfig() {
        super(new YamlProvider().loadConfiguration(new File("lobby.yml")));
    }
}
