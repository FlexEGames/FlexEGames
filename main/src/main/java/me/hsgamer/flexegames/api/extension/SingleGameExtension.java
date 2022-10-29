package me.hsgamer.flexegames.api.extension;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.builder.GameBuilder;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.hscore.common.Pair;
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
        GameBuilder.INSTANCE.register((s, pair) -> getInitializer().apply(pair), getId());
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
    public abstract Function<Pair<GameServer, Config>, Game> getInitializer();

    /**
     * Get the id (type) of the game
     *
     * @return the id
     */
    public abstract String[] getId();
}
