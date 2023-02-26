package me.hsgamer.flexegames.api.extension;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.builder.GameBuilder;
import net.minestom.server.extensions.Extension;

/**
 * An {@link Extension} to register a single game
 */
public abstract class SingleGameExtension extends Extension {
    @Override
    public final void initialize() {
        onEnable();
        GameBuilder.INSTANCE.register((s, config) -> getInitializer().apply(config), getIdentifier()); // TODO
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
     * Get the game
     *
     * @return the game
     */
    public abstract Game getGame();

    /**
     * Get the id (type) of the game
     *
     * @return the id
     */
    public abstract String getIdentifier();
}
