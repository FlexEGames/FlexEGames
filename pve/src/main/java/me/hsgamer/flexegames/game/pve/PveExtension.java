package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.util.ConfigUtil;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.config.Config;

import java.util.function.Function;

public class PveExtension extends SingleGameExtension {
    private final PveMessageConfig messageConfig = YamlConfigGenerator.generate(PveMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());

    @Override
    public Function<Config, Game> getInitializer() {
        return config -> {
            var gameConfig = ConfigUtil.getConfig(config, PveGameConfig.class);
            return new PveGame(this, gameConfig);
        };
    }

    @Override
    public String getIdentifier() {
        return "pve";
    }

    public PveMessageConfig getMessageConfig() {
        return messageConfig;
    }
}
