package me.hsgamer.flexegames.command.argument;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.game.Game;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import net.minestom.server.utils.binary.BinaryWriter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;

public class GameArgument extends Argument<Game> {
    public static final int GAME_NOT_FOUND = 1;
    private final GameServer gameServer;

    public GameArgument(GameServer gameServer, @NotNull String id) {
        super(id);
        this.gameServer = gameServer;
        setSuggestionCallback((sender, context, suggestion) -> {
            String raw = context.getRaw(this);
            gameServer.getGameManager().getGameMap().forEach((s, t) -> {
                if (raw == null || raw.isEmpty() || s.startsWith(raw)) {
                    suggestion.addEntry(new SuggestionEntry(s, t.getFeature(DescriptionFeature.class).getDisplayName()));
                }
            });
        });
    }

    @Override
    public @NotNull Game parse(@NotNull String input) throws ArgumentSyntaxException {
        Optional<Game> template = gameServer.getGameManager().getGame(input);
        if (template.isEmpty()) {
            throw new ArgumentSyntaxException("Template not found", input, GAME_NOT_FOUND);
        }
        return template.get();
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
        return String.format("Template<%s>", getId());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof GameArgument that)) return false;
        if (!super.equals(o)) return false;
        return gameServer.equals(that.gameServer);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), gameServer);
    }
}
