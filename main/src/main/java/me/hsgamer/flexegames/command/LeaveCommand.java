package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

/**
 * The command to leave the game
 */
public class LeaveCommand extends Command {
    public LeaveCommand(GameServer gameServer) {
        super("leave");
        setDefaultExecutor((sender, commandString) -> {
            if (sender instanceof Player player && !gameServer.getLobby().isInLobby(player)) {
                player.setInstance(gameServer.getLobby());
            }
        });
    }
}
