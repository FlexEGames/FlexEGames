package me.hsgamer.flexegames.game.duel;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.game.duel.manager.DuelWorldManager;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;
import me.hsgamer.flexegames.util.kit.GameKitManager;

@Getter
public class DuelExtension extends SingleGameExtension {
    private final DuelMessageConfig messageConfig = ConfigGeneratorUtil.generate(DuelMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());
    private final DuelMainConfig mainConfig = ConfigGeneratorUtil.generate(DuelMainConfig.class, getDataDirectory().resolve("config.yml").toFile());
    private final DuelWorldManager duelWorldManager = new DuelWorldManager(this);
    private final GameKitManager gameKitManager = new GameKitManager(ConfigGeneratorUtil.createConfig(getDataDirectory().resolve("kit.yml").toFile()), true);

    @Override
    public void onEnable() {
        duelWorldManager.init();
        gameKitManager.init();
    }

    @Override
    public void onDisable() {
        duelWorldManager.clear();
        gameKitManager.clear();
    }

    @Override
    public Game getGame() {
        return new DuelGame(this);
    }

    @Override
    public String getIdentifier() {
        return "duel";
    }
}
