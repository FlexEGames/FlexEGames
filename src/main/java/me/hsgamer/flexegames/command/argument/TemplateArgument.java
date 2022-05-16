package me.hsgamer.flexegames.command.argument;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.Template;
import net.kyori.adventure.text.Component;
import net.minestom.server.command.builder.NodeMaker;
import net.minestom.server.command.builder.arguments.Argument;
import net.minestom.server.command.builder.exception.ArgumentSyntaxException;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import net.minestom.server.network.packet.server.play.DeclareCommandsPacket;
import net.minestom.server.utils.binary.BinaryWriter;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;
import java.util.Optional;

public class TemplateArgument extends Argument<Template> {
    public static final int TEMPLATE_NOT_FOUND = 1;
    private final GameServer gameServer;

    public TemplateArgument(GameServer gameServer, @NotNull String id) {
        super(id);
        this.gameServer = gameServer;
        setSuggestionCallback((sender, context, suggestion) -> {
            String raw = context.getRaw(this);
            gameServer.getTemplateManager().getTemplateMap().forEach((s, t) -> {
                if (raw == null || raw.isBlank() || s.startsWith(raw)) {
                    Component component = Component.empty().append(t.getDisplayName()).append(Component.newline());
                    for (Component c : t.getDescription()) {
                        component = component.append(c).append(Component.newline());
                    }
                    suggestion.addEntry(new SuggestionEntry(s, component));
                }
            });
        });
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
        DeclareCommandsPacket.Node argumentNode = simpleArgumentNode(this, executable, false, false);

        argumentNode.parser = "brigadier:string";
        argumentNode.properties = BinaryWriter.makeArray(packetWriter -> {
            packetWriter.writeVarInt(1); // Quotable phrase
        });

        nodeMaker.addNodes(new DeclareCommandsPacket.Node[]{argumentNode});
    }

    @Override
    public String toString() {
        return String.format("Template<%s>", getId());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TemplateArgument that)) return false;
        if (!super.equals(o)) return false;
        return gameServer.equals(that.gameServer);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), gameServer);
    }
}
