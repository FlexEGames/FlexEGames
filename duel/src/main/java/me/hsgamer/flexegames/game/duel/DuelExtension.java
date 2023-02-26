package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.util.ConfigUtil;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.config.Config;

import java.util.function.Function;

public class DuelExtension extends SingleGameExtension {
    private final DuelMessageConfig messageConfig = YamlConfigGenerator.generate(DuelMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());

    @Override
    public Function<Config, Game> getInitializer() {
        return config -> {
            var gameConfig = ConfigUtil.getConfig(config, DuelGameConfig.class);
            return new DuelGame(this, gameConfig);
        };
    }

    @Override
    public String getIdentifier() {
        return "duel";
    }

    public DuelMessageConfig getMessageConfig() {
        return messageConfig;
    }
}
