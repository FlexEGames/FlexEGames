package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.Template;
import me.hsgamer.flexegames.command.argument.TemplateArgument;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

import java.util.Map;

public class CreateArenaCommand extends Command {
    public CreateArenaCommand(GameServer gameServer) {
        super("createarena", "create");
        setCondition((sender, commandString) -> sender instanceof Player player && gameServer.getLobby().isInLobby(player));
        setDefaultExecutor((sender, context) -> sender.sendMessage("Usage: /" + context.getCommandName() + " <template>"));

        var templateArgument = new TemplateArgument(gameServer, "template");
        setArgumentCallback((sender, exception) -> {
            if (exception.getErrorCode() == TemplateArgument.TEMPLATE_NOT_FOUND) {
                sender.sendMessage(ReplacementManager.replace(MessageConfig.ERROR_TEMPLATE_NOT_FOUND.getValue(), Map.of("input", () -> Component.text(exception.getInput()))));
            }
        }, templateArgument);
        addSyntax((sender, context) -> {
            Template template = context.get(templateArgument);
            Arena arena = gameServer.getGameArenaManager().createNewArena();
            arena.getArenaFeature(GameFeature.class).setGame(template);
            arena.getArenaFeature(GameFeature.class).setOwner(((Player) sender).getUuid());
        }, templateArgument);
        addSyntax((sender, context) -> gameServer.getLobby().openTemplateInventory((Player) sender));
    }
}
