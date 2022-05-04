package me.hsgamer.epicmegagames.command;

import me.hsgamer.epicmegagames.GameServer;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

public class LeaveCommand extends Command {
    public LeaveCommand(GameServer gameServer) {
        super("leave");
        setCondition((sender, commandString) -> sender instanceof Player player && !gameServer.getLobby().isInLobby(player));
        setDefaultExecutor((sender, commandString) -> ((Player) sender).setInstance(gameServer.getLobby()));
    }
}
