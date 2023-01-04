package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.command.argument.ArenaArgument;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.CommandSender;
import net.minestom.server.command.builder.Command;
import net.minestom.server.command.builder.arguments.ArgumentType;
import net.minestom.server.command.builder.suggestion.SuggestionEntry;
import net.minestom.server.entity.Player;
import net.minestom.server.utils.StringUtils;

import java.util.Map;
import java.util.function.Predicate;

/**
 * The command to join an arena
 */
public class JoinArenaCommand extends Command {
    public JoinArenaCommand(GameServer gameServer) {
        super("joinarena", "join");
        setDefaultExecutor((sender, context) -> {
            sender.sendMessage("Usage: /" + context.getCommandName() + " <arena>");
            sender.sendMessage("Usage: /" + context.getCommandName() + " search <owner>");
        });

        Predicate<CommandSender> playerLobbyPredicate = sender -> sender instanceof Player player && gameServer.getLobby().isInLobby(player);

        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            gameServer.getLobby().openArenaInventory((Player) sender, false);
        });

        ArenaArgument arenaArgument = new ArenaArgument(gameServer, "arena");
        setArgumentCallback((sender, exception) -> {
            if (exception.getErrorCode() == ArenaArgument.ARENA_NOT_FOUND) {
                sender.sendMessage(ReplacementManager.replace(gameServer.getMessageConfig().getErrorArenaNotFound(), Map.of("input", () -> Component.text(exception.getInput()))));
            }
        }, arenaArgument);
        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            Arena arena = context.get(arenaArgument);
            Player player = (Player) sender;
            gameServer.getLobby().tryJoinArena(player, arena);
        }, arenaArgument);

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
            if (!playerLobbyPredicate.test(sender)) return;
            String[] query = context.get(ownerQueryArgument);
            String queryString = String.join(" ", query);
            gameServer.getLobby().openArenaInventory((Player) sender, queryString);
        }, searchArgument, ownerQueryArgument);
    }
}
