package me.hsgamer.flexegames.game.pve;

import lombok.Getter;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.helper.kit.KitManager;
import me.hsgamer.flexegames.util.ConfigGeneratorUtil;

@Getter
public class PveExtension extends SingleGameExtension {
    private final PveMessageConfig messageConfig = ConfigGeneratorUtil.generate(PveMessageConfig.class, getDataDirectory().resolve("messages.yml").toFile());
    private final PveMainConfig mainConfig = ConfigGeneratorUtil.generate(PveMainConfig.class, getDataDirectory().resolve("config.yml").toFile());
    private final KitManager kitManager = new KitManager(ConfigGeneratorUtil.createConfig(getDataDirectory().resolve("kit.yml").toFile()), true);
    private final PvePropertyEditor propertyEditor = new PvePropertyEditor(this);

    @Override
    public void onEnable() {
        kitManager.init();
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
