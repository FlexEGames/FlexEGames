package me.hsgamer.flexegames.builder;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.config.Config;

import java.util.Optional;

/**
 * The builder for {@link Game}
 */
// TODO: Remove this class
public class GameBuilder extends Builder<Config, Game> {
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
     * @param config the config
     * @return the game
     */
    public static Optional<Game> buildGame(Config config) {
        return Optional.ofNullable(config.get("type"))
                .map(String::valueOf)
                .flatMap(type -> GameBuilder.INSTANCE.build(type, config));
    }
}
