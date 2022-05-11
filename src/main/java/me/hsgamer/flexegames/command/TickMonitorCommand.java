package me.hsgamer.flexegames.command;

import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.manager.ReplacementManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.command.ConsoleSender;
import net.minestom.server.command.builder.Command;

import java.util.List;

public class TickMonitorCommand extends Command {
    public TickMonitorCommand() {
        super("tickmonitor", "monitor");
        setCondition((sender, commandString) -> sender instanceof ConsoleSender);
        setDefaultExecutor((sender, context) -> {
            List<Component> components = MessageConfig.TICK_MONITOR.getValue();
            components.replaceAll(ReplacementManager::replaceGlobal);
            for (Component component : components) {
                sender.sendMessage(component);
            }
        });
    }
}
