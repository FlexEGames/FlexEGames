package me.hsgamer.epicmegagames.command.argument;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.epicmegagames.api.Template;
import net.minestom.server.command.builder.NodeMaker;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.network.packet.server.play.DeclareCommandsPacket;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class TemplateArgument extends Argument<Template> {
    public static final int TEMPLATE_NOT_FOUND = 1;
    private final GameServer gameServer;

    public TemplateArgument(GameServer gameServer, @NotNull String id) {
        super(id);
        this.gameServer = gameServer;
    }

    @Override
    public @NotNull Template parse(@NotNull String input) throws ArgumentSyntaxException {
        Optional<Template> template = gameServer.getTemplateManager().getTemplate(input);
        if (template.isEmpty()) {
            throw new ArgumentSyntaxException("Template not found", input, TEMPLATE_NOT_FOUND);
        }
        return template.get();
    }

    @Override
    public void processNodes(@NotNull NodeMaker nodeMaker, boolean executable) {
        List<DeclareCommandsPacket.Node> list = new ArrayList<>();
        gameServer.getTemplateManager().getTemplateMap().forEach((key, value) -> {
            DeclareCommandsPacket.Node argumentNode = new DeclareCommandsPacket.Node();
            argumentNode.flags = DeclareCommandsPacket.getFlag(DeclareCommandsPacket.NodeType.LITERAL, executable, false, false);
            argumentNode.name = key;
            list.add(argumentNode);
        });
        nodeMaker.addNodes(list.toArray(new DeclareCommandsPacket.Node[0]));
    }

    @Override
    public String toString() {
        return String.format("Template<%s>", getId());
    }
}
