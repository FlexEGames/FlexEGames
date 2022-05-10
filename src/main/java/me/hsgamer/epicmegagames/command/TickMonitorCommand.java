package me.hsgamer.epicmegagames.command;

import me.hsgamer.epicmegagames.config.MessageConfig;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
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
