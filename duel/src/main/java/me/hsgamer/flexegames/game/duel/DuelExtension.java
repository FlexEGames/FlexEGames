package me.hsgamer.flexegames.game.duel;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.game.duel.manager.DuelWorldManager;
import me.hsgamer.flexegames.helper.kit.KitManager;
import me.hsgamer.flexegames.util.ConfigUtil;

@Getter
public class DuelExtension extends SingleGameExtension {
    private final DuelMessageConfig messageConfig = ConfigUtil.generate(DuelMessageConfig.class, ConfigUtil.getConfigFile(getDataDirectory().toFile(), "messages"));
    private final DuelMainConfig mainConfig = ConfigUtil.generate(DuelMainConfig.class, ConfigUtil.getConfigFile(getDataDirectory().toFile(), "config"));
    private final DuelWorldManager duelWorldManager = new DuelWorldManager(this);
    private final KitManager kitManager = new KitManager();
    private final DuelPropertyEditor propertyEditor = new DuelPropertyEditor(this);

    @Override
    public void onEnable() {
        duelWorldManager.init();
        kitManager.loadFromConfig(ConfigUtil.createConfig(ConfigUtil.getConfigFile(getDataDirectory().toFile(), "kit")), true);
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
