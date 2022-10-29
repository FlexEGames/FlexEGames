package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.GameServer;
import net.minestom.server.command.ConsoleSender;
import net.minestom.server.command.builder.Command;

/**
 * The command to stop the server
 */
public class StopCommand extends Command {
    public StopCommand(GameServer gameServer) {
        super("stop");
        setCondition((sender, commandString) -> sender instanceof ConsoleSender);
        setDefaultExecutor((sender, context) -> gameServer.stop());
    }
}
