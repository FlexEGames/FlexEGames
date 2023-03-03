package me.hsgamer.flexegames.game.pve;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.helper.kit.KitManager;
import me.hsgamer.flexegames.helper.kit.SimpleKit;
import me.hsgamer.flexegames.util.ConfigUtil;

@Getter
public class PveExtension extends SingleGameExtension {
    private final PveMessageConfig messageConfig = ConfigUtil.generate(PveMessageConfig.class, ConfigUtil.getConfigFile(getDataDirectory().toFile(), "messages"));
    private final PveMainConfig mainConfig = ConfigUtil.generate(PveMainConfig.class, ConfigUtil.getConfigFile(getDataDirectory().toFile(), "config"));
    private final KitManager kitManager = new KitManager();
    private final PvePropertyEditor propertyEditor = new PvePropertyEditor(this);

    @Override
    public void onEnable() {
        kitManager.loadFromConfig(ConfigUtil.createConfig(ConfigUtil.getConfigFile(getDataDirectory().toFile(), "kit")), true);
        kitManager.setDefaultKit(new SimpleKit(mainConfig.getDefaultKit()));
        propertyEditor.init();
    }

    @Override
    public void onDisable() {
        propertyEditor.stop();
        kitManager.clear();
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
