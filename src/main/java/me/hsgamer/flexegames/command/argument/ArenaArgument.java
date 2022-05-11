package me.hsgamer.flexegames.command.argument;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.command.builder.NodeMaker;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import net.minestom.server.network.packet.server.play.DeclareCommandsPacket;
import net.minestom.server.utils.binary.BinaryWriter;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public class ArenaArgument extends Argument<Arena> {
    public static final int ARENA_NOT_FOUND = 1;
    public static final int ARENA_NOT_SETUP = 2;
    private final GameServer gameServer;

    public ArenaArgument(GameServer gameServer, @NotNull String id) {
        super(id);
        this.gameServer = gameServer;
        setSuggestionCallback((sender, context, suggestion) -> {
            String raw = context.getRaw(this);
            gameServer.getGameArenaManager().getAllArenas().forEach(arena -> {
                if (arena.getArenaFeature(GameFeature.class).getGame() == null) {
                    return;
                }
                String s = arena.getName();
                if (raw == null || raw.isBlank() || s.startsWith(raw)) {
                    suggestion.addEntry(new SuggestionEntry(s));
                }
            });
        });
    }

    @Override
    public @NotNull Arena parse(@NotNull String input) throws ArgumentSyntaxException {
        Optional<Arena> optional = gameServer.getGameArenaManager().getArenaByName(input);
        if (optional.isEmpty()) {
            throw new ArgumentSyntaxException("Arena not found", input, ARENA_NOT_FOUND);
        }
        Arena arena = optional.get();
        if (arena.getArenaFeature(GameFeature.class).getGame() == null) {
            throw new ArgumentSyntaxException("Arena not setup", input, ARENA_NOT_SETUP);
        }
        return arena;
    }

    @Override
    public void processNodes(@NotNull NodeMaker nodeMaker, boolean executable) {
        DeclareCommandsPacket.Node argumentNode = simpleArgumentNode(this, executable, false, false);

        argumentNode.parser = "brigadier:string";
        argumentNode.properties = BinaryWriter.makeArray(packetWriter -> {
            packetWriter.writeVarInt(1); // Quotable phrase
        });

        nodeMaker.addNodes(new DeclareCommandsPacket.Node[]{argumentNode});
    }
}
