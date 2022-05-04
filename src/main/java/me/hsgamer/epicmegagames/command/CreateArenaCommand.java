package me.hsgamer.epicmegagames.command;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.command.argument.TemplateArgument;
import me.hsgamer.epicmegagames.feature.GameFeature;
import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

public class CreateArenaCommand extends Command {
    public CreateArenaCommand(GameServer gameServer) {
        super("createarena");
        setCondition((sender, commandString) -> sender instanceof Player player && player.getInstance() == gameServer.getLobby());
        setDefaultExecutor((sender, context) -> sender.sendMessage("Usage: /createarena <template>"));

        var templateArgument = new TemplateArgument(gameServer, "template");
        addSyntax((sender, context) -> {
            Template template = context.get(templateArgument);
            Arena arena = gameServer.getGameArenaManager().createNewArena();
            arena.getArenaFeature(GameFeature.class).setGame(template);
            arena.getArenaFeature(GameFeature.class).setOwner(((Player) sender).getUuid());
        }, templateArgument);
    }
}
