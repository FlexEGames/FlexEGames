package me.hsgamer.flexegames.api.extension;

import me.hsgamer.flexegames.api.game.Template;
import me.hsgamer.flexegames.builder.TemplateBuilder;
import me.hsgamer.hscore.config.Config;
import net.minestom.server.extensions.Extension;

import java.util.function.Function;

public abstract class SingleTemplateExtension extends Extension {
    @Override
    public final void initialize() {
        onEnable();
        TemplateBuilder.INSTANCE.register(getInitializer(), getId(), getAliases());
    }

    @Override
    public final void terminate() {
        onDisable();
    }

    public void onEnable() {
        // EMPTY
    }

    public void onDisable() {
        // EMPTY
    }

    public abstract Function<Config, Template> getInitializer();

    public abstract String getId();

    public String[] getAliases() {
        return new String[0];
    }
}
