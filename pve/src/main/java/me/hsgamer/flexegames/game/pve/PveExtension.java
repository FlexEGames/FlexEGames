package me.hsgamer.flexegames.game.pve;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;

@Getter
public class PveExtension extends SingleGameExtension {
    private final PveMessageConfig messageConfig = ConfigGeneratorUtil.generate(PveMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());
    private final PveMainConfig mainConfig = ConfigGeneratorUtil.generate(PveMainConfig.class, getDataDirectory().resolve("config.yml").toFile());

    @Override
    public Game getGame() {
        return null;
    }

    @Override
    public String getIdentifier() {
        return "pve";
    }
}
