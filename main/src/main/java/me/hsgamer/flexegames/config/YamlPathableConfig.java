package me.hsgamer.flexegames.config;

import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;

import java.io.File;

public class YamlPathableConfig extends PathableConfig {
    public YamlPathableConfig(File file) {
        super(new YamlProvider().loadConfiguration(file));
    }
}
