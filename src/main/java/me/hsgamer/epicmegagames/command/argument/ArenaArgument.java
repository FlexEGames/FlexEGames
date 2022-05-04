package me.hsgamer.epicmegagames.command.argument;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.command.builder.NodeMaker;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.network.packet.server.play.DeclareCommandsPacket;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class ArenaArgument extends Argument<Arena> {
    public static final int ARENA_NOT_FOUND = 1;
    private final GameServer gameServer;

    public ArenaArgument(GameServer gameServer, @NotNull String id) {
        super(id);
        this.gameServer = gameServer;
    }

    @Override
    public @NotNull Arena parse(@NotNull String input) throws ArgumentSyntaxException {
        Optional<Arena> arena = gameServer.getGameArenaManager().getArenaByName(input);
        if (arena.isEmpty()) {
            throw new ArgumentSyntaxException("Arena not found", input, ARENA_NOT_FOUND);
        }
        return arena.get();
    }

    @Override
    public void processNodes(@NotNull NodeMaker nodeMaker, boolean executable) {
        List<DeclareCommandsPacket.Node> list = new ArrayList<>();
        gameServer.getGameArenaManager().getAllArenas().forEach(arena -> {
            DeclareCommandsPacket.Node argumentNode = new DeclareCommandsPacket.Node();
            argumentNode.flags = DeclareCommandsPacket.getFlag(DeclareCommandsPacket.NodeType.LITERAL, executable, false, false);
            argumentNode.name = arena.getName();
            list.add(argumentNode);
        });
        nodeMaker.addNodes(list.toArray(new DeclareCommandsPacket.Node[0]));
    }
}
