package me.hsgamer.flexegames.game.duel;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.game.duel.manager.DuelWorldManager;
import me.hsgamer.flexegames.helper.kit.KitManager;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;

@Getter
public class DuelExtension extends SingleGameExtension {
    private final DuelMessageConfig messageConfig = ConfigGeneratorUtil.generate(DuelMessageConfig.class, ConfigGeneratorUtil.getConfigFile(getDataDirectory().toFile(), "messages"));
    private final DuelMainConfig mainConfig = ConfigGeneratorUtil.generate(DuelMainConfig.class, ConfigGeneratorUtil.getConfigFile(getDataDirectory().toFile(), "config"));
    private final DuelWorldManager duelWorldManager = new DuelWorldManager(this);
    private final KitManager kitManager = new KitManager(ConfigGeneratorUtil.createConfig(ConfigGeneratorUtil.getConfigFile(getDataDirectory().toFile(), "kit")), true);
    private final DuelPropertyEditor propertyEditor = new DuelPropertyEditor(this);

    @Override
    public void onEnable() {
        duelWorldManager.init();
        kitManager.init();
        propertyEditor.init();
    }

    @Override
    public void onDisable() {
        propertyEditor.stop();
        duelWorldManager.clear();
        kitManager.clear();
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
