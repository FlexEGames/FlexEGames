package me.hsgamer.epicmegagames.command;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.command.argument.TemplateArgument;
import me.hsgamer.epicmegagames.config.MessageConfig;
import me.hsgamer.epicmegagames.feature.GameFeature;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

import java.util.Map;

public class CreateArenaCommand extends Command {
    public CreateArenaCommand(GameServer gameServer) {
        super("createarena");
        setCondition((sender, commandString) -> sender instanceof Player player && player.getInstance() == gameServer.getLobby());
        setDefaultExecutor((sender, context) -> sender.sendMessage("Usage: /createarena <template>"));

        var templateArgument = new TemplateArgument(gameServer, "template");
        templateArgument.setCallback((sender, exception) -> {
            if (exception.getErrorCode() == TemplateArgument.TEMPLATE_NOT_FOUND) {
                sender.sendMessage(ReplacementManager.replace(MessageConfig.ERROR_TEMPLATE_NOT_FOUND.getValue(), Map.of("input", Component.text(exception.getInput()))));
            }
        });
        addSyntax((sender, context) -> {
            Template template = context.get(templateArgument);
            Arena arena = gameServer.getGameArenaManager().createNewArena();
            arena.getArenaFeature(GameFeature.class).setGame(template);
            arena.getArenaFeature(GameFeature.class).setOwner(((Player) sender).getUuid());
        }, templateArgument);
    }
}