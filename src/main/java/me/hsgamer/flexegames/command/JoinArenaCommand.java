package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.ArenaGame;
import me.hsgamer.flexegames.api.JoinResponse;
import me.hsgamer.flexegames.command.argument.ArenaArgument;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

import java.util.Map;

public class JoinArenaCommand extends Command {
    public JoinArenaCommand(GameServer gameServer) {
        super("joinarena", "join");
        setCondition((sender, commandString) -> sender instanceof Player player && gameServer.getLobby().isInLobby(player));
        setDefaultExecutor((sender, context) -> sender.sendMessage("Usage: /" + context.getCommandName() + " <arena>"));
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
    }
}
