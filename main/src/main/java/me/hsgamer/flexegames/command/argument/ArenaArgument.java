package me.hsgamer.flexegames.command.argument;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import net.minestom.server.utils.binary.BinaryWriter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;

/**
 * An argument for {@link Arena}
 */
public class ArenaArgument extends Argument<Arena> {
    public static final int ARENA_NOT_FOUND = 1;
    private final GameServer gameServer;

    public ArenaArgument(GameServer gameServer, @NotNull String id) {
        super(id);
        this.gameServer = gameServer;

        setSuggestionCallback((sender, context, suggestion) -> {
            String raw = context.getRaw(this);
            gameServer.getArenaManager().getAllArenas().forEach(arena -> {
                String s = arena.getName();
                if (raw == null || raw.isEmpty() || s.startsWith(raw)) {
                    suggestion.addEntry(new SuggestionEntry(s));
                }
            });
        });
    }

    @Override
    public @NotNull Arena parse(@NotNull String input) throws ArgumentSyntaxException {
        Optional<Arena> arena = gameServer.getArenaManager().getArenaByName(input);
        if (arena.isEmpty()) {
            throw new ArgumentSyntaxException("Arena not found", input, ARENA_NOT_FOUND);
        }
        return arena.get();
    }

    @Override
    public String parser() {
        return "brigadier:string";
    }

    @Override
    public byte @Nullable [] nodeProperties() {
        return BinaryWriter.makeArray(packetWriter -> packetWriter.writeVarInt(0));
    }

    @Override
    public String toString() {
        return String.format("Arena<%s>", getId());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ArenaArgument that)) return false;
        if (!super.equals(o)) return false;
        return Objects.equals(gameServer, that.gameServer);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), gameServer);
    }
}
