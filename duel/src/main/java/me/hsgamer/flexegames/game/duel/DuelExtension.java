package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;

public class DuelExtension extends SingleGameExtension {
    private final DuelMessageConfig messageConfig = ConfigGeneratorUtil.generate(DuelMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());

    @Override
    public Game getGame() {
        return null;
    }

    @Override
    public String getIdentifier() {
        return "duel";
    }

    public DuelMessageConfig getMessageConfig() {
        return messageConfig;
    }
}
