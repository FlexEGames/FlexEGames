package me.hsgamer.flexgames.command;

import me.hsgamer.flexgames.GameServer;
import net.minestom.server.command.ConsoleSender;
import net.minestom.server.command.builder.Command;

public class StopCommand extends Command {
    public StopCommand(GameServer gameServer) {
        super("stop");
        setCondition((sender, commandString) -> sender instanceof ConsoleSender);
        setDefaultExecutor((sender, context) -> gameServer.stop());
    }
}
