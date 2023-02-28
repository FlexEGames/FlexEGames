package me.hsgamer.flexegames.game.pve;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;
import me.hsgamer.flexegames.util.kit.GameKitManager;

@Getter
public class PveExtension extends SingleGameExtension {
    private final PveMessageConfig messageConfig = ConfigGeneratorUtil.generate(PveMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());
    private final PveMainConfig mainConfig = ConfigGeneratorUtil.generate(PveMainConfig.class, getDataDirectory().resolve("config.yml").toFile());
    private final GameKitManager gameKitManager = new GameKitManager(ConfigGeneratorUtil.createConfig(getDataDirectory().resolve("kit.yml").toFile()), true);

    @Override
    public void onEnable() {
        gameKitManager.init();
    }

    @Override
    public void onDisable() {
        gameKitManager.clear();
    }

    @Override
    public Game getGame() {
        return new PveGame(this);
    }

    @Override
    public String getIdentifier() {
        return "pve";
    }
}
