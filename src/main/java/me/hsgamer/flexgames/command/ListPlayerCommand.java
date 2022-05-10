package me.hsgamer.flexgames.command;

import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.command.ConsoleSender;
import net.minestom.server.command.builder.Command;
import net.minestom.server.entity.Player;

import java.util.Collection;

public class ListPlayerCommand extends Command {
    public ListPlayerCommand() {
        super("listplayer", "list");
        setCondition((sender, commandString) -> sender instanceof ConsoleSender);
        setDefaultExecutor((sender, context) -> {
            Collection<Player> players = MinecraftServer.getConnectionManager().getOnlinePlayers();
            Component component = Component.text("Players ")
                    .append(Component.text("(" + players.size() + ")"))
                    .append(Component.text(": "));
            for (Player player : MinecraftServer.getConnectionManager().getOnlinePlayers()) {
                component = component.append(player.getName()).append(Component.text(", "));
            }
            sender.sendMessage(component);
        });
    }
}
