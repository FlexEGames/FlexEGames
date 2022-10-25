package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.util.YamlConfigGenerator;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;

import java.util.function.Function;

public class PveExtension extends SingleGameExtension {
    private final PveMessageConfig messageConfig = YamlConfigGenerator.generate(PveMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());

    @Override
    public Function<Pair<GameServer, Config>, Game> getInitializer() {
        return pair -> new PveGame(pair, this);
    }

    @Override
    public String[] getId() {
        return new String[]{"pve"};
    }

    public PveMessageConfig getMessageConfig() {
        return messageConfig;
    }
}
