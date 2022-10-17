package me.hsgamer.flexegames.command.argument;

import me.hsgamer.flexegames.game.Game;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;
import java.util.function.Function;

public class ArenaArgument extends Argument<Function<Game, Optional<Arena>>> {
    public ArenaArgument(@NotNull String id) {
        super(id);
    }

    public void setGameArgument(GameArgument gameArgument) {
        setSuggestionCallback((sender, context, suggestion) -> {
            Game game = context.get(gameArgument);
            if (game == null) return;
            String raw = context.getRaw(this);
            game.getAllArenas().forEach(arena -> {
                String s = arena.getName();
                if (raw == null || raw.isBlank() || s.startsWith(raw)) {
                    suggestion.addEntry(new SuggestionEntry(s));
                }
            });
        });
    }

    @Override
    public @NotNull Function<Game, Optional<Arena>> parse(@NotNull String input) throws ArgumentSyntaxException {
        return game -> game.getArenaByName(input);
    }

    @Override
    public String parser() {
        return null;
    }

    @Override
    public String toString() {
        return String.format("Arena<%s>", getId());
    }
}
