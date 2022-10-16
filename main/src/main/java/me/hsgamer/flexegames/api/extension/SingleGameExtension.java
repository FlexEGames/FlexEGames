package me.hsgamer.flexegames.api.extension;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.builder.GameBuilder;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;
import net.minestom.server.extensions.Extension;

import java.util.function.Function;

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

    public void onEnable() {
        // EMPTY
    }

    public void onDisable() {
        // EMPTY
    }

    public abstract Function<Pair<GameServer, Config>, Game> getInitializer();

    public abstract String[] getId();
}
