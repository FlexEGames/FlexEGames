package me.hsgamer.epicmegagames.command;

import me.hsgamer.epicmegagames.GameServer;
import me.hsgamer.epicmegagames.api.ArenaGame;
import me.hsgamer.epicmegagames.api.JoinResponse;
import me.hsgamer.epicmegagames.command.argument.ArenaArgument;
import me.hsgamer.epicmegagames.config.MessageConfig;
import me.hsgamer.epicmegagames.feature.GameFeature;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
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
