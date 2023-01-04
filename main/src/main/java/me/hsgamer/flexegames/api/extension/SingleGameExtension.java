package me.hsgamer.flexegames.api.extension;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.builder.GameBuilder;
import me.hsgamer.hscore.config.Config;
import net.minestom.server.extensions.Extension;

import java.util.function.Function;

/**
 * An {@link Extension} to register a single game
 */
public abstract class SingleGameExtension extends Extension {
    @Override
    public final void initialize() {
        onEnable();
        GameBuilder.INSTANCE.register((s, config) -> getInitializer().apply(config), getId());
    }

    @Override
    public final void terminate() {
        onDisable();
    }

    /**
     * Called when the extension is enabled
     */
    public void onEnable() {
        // EMPTY
    }

    /**
     * Called when the extension is disabled
     */
    public void onDisable() {
        // EMPTY
    }

    /**
     * Get the initializer that registers the game
     *
     * @return the initializer
     */
    public abstract Function<Config, Game> getInitializer();

    /**
     * Get the id (type) of the game
     *
     * @return the id
     */
    public abstract String[] getId();
}
