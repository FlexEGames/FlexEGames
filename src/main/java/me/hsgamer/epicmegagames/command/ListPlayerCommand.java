package me.hsgamer.epicmegagames.command;

import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.ConsoleSender;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

public class ListPlayerCommand extends Command {
    public ListPlayerCommand() {
        super("listplayer", "list");
        setCondition((sender, commandString) -> sender instanceof ConsoleSender);
        setDefaultExecutor((sender, context) -> {
            Component component = Component.text("Players: ");
            for (Player player : MinecraftServer.getConnectionManager().getOnlinePlayers()) {
                component = component.append(player.getName()).append(Component.text(", "));
            }
            sender.sendMessage(component);
        });
    }
}
