package me.hsgamer.flexegames.builder;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;

import java.util.Optional;

/**
 * The builder for {@link Game}
 */
public class GameBuilder extends Builder<Pair<GameServer, Config>, Game> {
    /**
     * The singleton instance
     */
    public static final GameBuilder INSTANCE = new GameBuilder();

    private GameBuilder() {
        super();
    }

    /**
     * Build the game from the config
     *
     * @param gameServer the game server
     * @param config     the config
     * @return the game
     */
    public static Optional<Game> buildGame(GameServer gameServer, Config config) {
        return Optional.ofNullable(config.get("type"))
                .map(String::valueOf)
                .flatMap(type -> GameBuilder.INSTANCE.build(type, Pair.of(gameServer, config)));
    }
}
