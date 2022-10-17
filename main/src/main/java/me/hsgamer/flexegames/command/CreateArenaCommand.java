package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.command.argument.GameArgument;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.manager.ReplacementManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.command.CommandSender;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

import java.util.Map;
import java.util.function.Predicate;

public class CreateArenaCommand extends Command {
    public CreateArenaCommand(GameServer gameServer) {
        super("createarena", "create");
        setCondition((sender, commandString) -> sender instanceof Player player && gameServer.getLobby().isInLobby(player));
        setDefaultExecutor((sender, context) -> sender.sendMessage("Usage: /" + context.getCommandName() + " <game>"));

        Predicate<CommandSender> playerLobbyPredicate = sender -> sender instanceof Player player && gameServer.getLobby().isInLobby(player);

        var gameArgument = new GameArgument(gameServer, "template");
        setArgumentCallback((sender, exception) -> {
            if (exception.getErrorCode() == GameArgument.GAME_NOT_FOUND) {
                sender.sendMessage(ReplacementManager.replace(gameServer.getMessageConfig().getErrorGameNotFound(), Map.of("input", () -> Component.text(exception.getInput()))));
            }
        }, gameArgument);
        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            Game game = context.get(gameArgument);
            game.createArena(((Player) sender).getUuid());
            sender.sendMessage(gameServer.getMessageConfig().getResponseCreateArenaSuccessful());
        }, gameArgument);
        addSyntax((sender, context) -> {
            if (!playerLobbyPredicate.test(sender)) return;
            gameServer.getLobby().openGameInventory((Player) sender);
        });
    }
}
