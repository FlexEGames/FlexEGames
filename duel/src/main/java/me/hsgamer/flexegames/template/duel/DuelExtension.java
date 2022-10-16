package me.hsgamer.flexegames.template.duel;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;

import java.util.function.Function;

public class DuelExtension extends SingleGameExtension {
    private final DuelMessageConfig messageConfig = YamlConfigGenerator.generate(DuelMessageConfig.class, getDataDirectory().resolve("message.yml").toFile());

    @Override
    public Function<Pair<GameServer, Config>, Game> getInitializer() {
        return pair -> new DuelGame(pair, this);
    }

    @Override
    public String[] getId() {
        return new String[]{"duel"};
    }

    public DuelMessageConfig getMessageConfig() {
        return messageConfig;
    }
}
