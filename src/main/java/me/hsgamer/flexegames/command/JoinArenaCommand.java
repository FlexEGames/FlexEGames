package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.ArenaGame;
import me.hsgamer.flexegames.api.JoinResponse;
import me.hsgamer.flexegames.api.Template;
import me.hsgamer.flexegames.command.argument.ArenaArgument;
import me.hsgamer.flexegames.command.argument.TemplateArgument;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.builder.Command;
import net.minestom.server.command.builder.arguments.ArgumentType;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import net.minestom.server.entity.Player;
import net.minestom.server.utils.StringUtils;

import java.util.Map;

public class JoinArenaCommand extends Command {
    public JoinArenaCommand(GameServer gameServer) {
        super("joinarena", "join");
        setCondition((sender, commandString) -> sender instanceof Player player && gameServer.getLobby().isInLobby(player));
        setDefaultExecutor((sender, context) -> {
            sender.sendMessage("Usage: /" + context.getCommandName() + " <arena>");
            sender.sendMessage("Usage: /" + context.getCommandName() + " search <owner>");
            sender.sendMessage("Usage: /" + context.getCommandName() + " template <template>");
        });

        var arenaArgument = new ArenaArgument(gameServer, "arena");
        setArgumentCallback((sender, exception) -> {
            if (exception.getErrorCode() == ArenaArgument.ARENA_NOT_FOUND) {
                sender.sendMessage(ReplacementManager.replace(MessageConfig.ERROR_ARENA_NOT_FOUND.getValue(), Map.of("input", () -> Component.text(exception.getInput()))));
            } else if (exception.getErrorCode() == ArenaArgument.ARENA_NOT_SETUP) {
                sender.sendMessage(ReplacementManager.replace(MessageConfig.ERROR_ARENA_NOT_SETUP.getValue(), Map.of("input", () -> Component.text(exception.getInput()))));
            }
        }, arenaArgument);

        addSyntax((sender, context) -> {
            Arena arena = context.get(arenaArgument);
            ArenaGame arenaGame = arena.getArenaFeature(GameFeature.class).getGame();
            JoinResponse response = arenaGame.join((Player) sender);
            if (!response.success()) {
                sender.sendMessage(response.getMessage((Player) sender));
            }
        }, arenaArgument);

        addSyntax((sender, context) -> gameServer.getLobby().openArenaInventory((Player) sender, false));

        var searchArgument = ArgumentType.Literal("search");
        var ownerQueryArgument = ArgumentType.StringArray("owner");
        ownerQueryArgument.setSuggestionCallback((sender, context, suggestion) -> {
            String[] args = context.get(ownerQueryArgument);
            if (args == null || args.length < 1) {
                MinecraftServer.getConnectionManager().getOnlinePlayers().forEach(player -> suggestion.addEntry(new SuggestionEntry(player.getUsername(), player.getName())));
            } else if (args.length == 1) {
                MinecraftServer.getConnectionManager().getOnlinePlayers().forEach(player -> {
                    if (StringUtils.jaroWinklerScore(player.getUsername(), args[0]) > 0) {
                        suggestion.addEntry(new SuggestionEntry(player.getUsername(), player.getName()));
                    }
                });
            }
        });
        addSyntax((sender, context) -> {
            String[] query = context.get(ownerQueryArgument);
            String queryString = String.join(" ", query);
            gameServer.getLobby().openArenaInventory((Player) sender, queryString);
        }, searchArgument, ownerQueryArgument);

        var templateArgument = ArgumentType.Literal("template");
        var templateNameArgument = new TemplateArgument(gameServer, "templateName");
        addSyntax((sender, context) -> {
            Template template = context.get(templateNameArgument);
            gameServer.getLobby().openArenaInventory((Player) sender, () -> gameServer.getGameArenaManager().findArenasByTemplate(template));
        }, templateArgument, templateNameArgument);
    }
}
