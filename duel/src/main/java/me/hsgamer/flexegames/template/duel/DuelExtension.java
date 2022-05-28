package me.hsgamer.flexegames.template.duel;

import me.hsgamer.flexegames.api.extension.SingleTemplateExtension;
import me.hsgamer.flexegames.api.game.Template;
import me.hsgamer.hscore.config.Config;

import java.util.function.Function;

public class DuelExtension extends SingleTemplateExtension {
    @Override
    public void onEnable() {
        new DuelMessageConfig(this).setup();
    }

    @Override
    public Function<Config, Template> getInitializer() {
        return DuelTemplate::new;
    }

    @Override
    public String getId() {
        return "duel";
    }
}
