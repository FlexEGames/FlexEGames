package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.config.path.ComponentPath;
import me.hsgamer.hscore.config.PathableConfig;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.simplixstorage.YamlProvider;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;

import java.io.File;

public class ChatConfig extends PathableConfig {
    public static final ConfigPath<Component> CHAT_FORMAT = new ComponentPath("chat-format",
            Component.empty()
                    .append(Component.text("%player%").color(NamedTextColor.WHITE))
                    .append(Component.text(": ").color(NamedTextColor.YELLOW))
                    .append(Component.text("%message%").color(NamedTextColor.GOLD))
    );

    public ChatConfig() {
        super(new YamlProvider().loadConfiguration(new File("chat.yml")));
    }
}
