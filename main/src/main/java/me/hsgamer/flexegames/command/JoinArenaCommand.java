package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.command.argument.ArenaArgument;
import me.hsgamer.flexegames.command.argument.GameArgument;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.Game;
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
import java.util.Optional;
import java.util.function.Predicate;

public class JoinArenaCommand extends Command {
    public JoinArenaCommand(GameServer gameServer) {
        super("joinarena", "join");
        setDefaultExecutor((sender, context) -> {
            sender.sendMessage("Usage: /" + context.getCommandName() + " <game>");
            sender.sendMessage("Usage: /" + context.getCommandName() + " <game> <arena>");
            sender.sendMessage("Usage: /" + context.getCommandName() + " search <owner>");
        });

        Predicate<CommandSender> playerLobbyPredicate = sender -> sender instanceof Player player && gameServer.getLobby().isInLobby(player);

        var gameArgument = new GameArgument(gameServer, "game");
        setArgumentCallback((sender, exception) -> {
            if (exception.getErrorCode() == GameArgument.GAME_NOT_FOUND) {
                sender.sendMessage(ReplacementManager.replace(gameServer.getMessageConfig().getErrorGameNotFound(), Map.of("input", () -> Component.text(exception.getInput()))));
            }
        }, gameArgument);

        var arenaArgument = new ArenaArgument("arena");
        arenaArgument.setGameArgument(gameArgument);

        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            Game game = context.get(gameArgument);
            Optional<Arena> optionalArena = context.get(arenaArgument).apply(game);
            if (optionalArena.isEmpty()) {
                sender.sendMessage(ReplacementManager.replace(gameServer.getMessageConfig().getErrorArenaNotFound(), Map.of("input", () -> Component.text(context.getInput()))));
                return;
            }
            Player player = (Player) sender;
            Arena arena = optionalArena.get();
            var joinFeature = arena.getArenaFeature(JoinFeature.class);
            if (joinFeature.isJoined(player)) {
                sender.sendMessage(gameServer.getMessageConfig().getErrorArenaJoined());
                return;
            }
            JoinResponse response = optionalArena.get().getArenaFeature(JoinFeature.class).join((Player) sender);
            if (!response.success()) {
                sender.sendMessage(response.message());
            }
        }, gameArgument, arenaArgument);

        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            gameServer.getLobby().openArenaInventory((Player) sender, false);
        });

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

        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            Game game = context.get(gameArgument);
            gameServer.getLobby().openArenaInventory((Player) sender, game::getAllArenas);
        }, gameArgument);
    }
}
