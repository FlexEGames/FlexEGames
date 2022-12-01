package me.hsgamer.flexegames.command;

import net.minestom.server.command.ConsoleSender;
import net.minestom.server.command.builder.Command;

/**
 * The command to stop the server
 */
public class StopCommand extends Command {
    public StopCommand() {
        super("stop");
        setCondition((sender, commandString) -> sender instanceof ConsoleSender);
        setDefaultExecutor((sender, context) -> System.exit(0));
    }
}
