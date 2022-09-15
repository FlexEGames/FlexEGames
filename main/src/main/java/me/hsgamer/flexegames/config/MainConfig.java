package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.FlexEGames;
import me.hsgamer.flexegames.config.path.ComponentListPath;
import me.hsgamer.flexegames.config.path.ComponentMapPath;
import me.hsgamer.flexegames.config.path.PermissionListMapPath;
import me.hsgamer.flexegames.config.path.PermissionListPath;
import me.hsgamer.flexegames.util.ProxyType;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.permission.Permission;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.*;

public class MainConfig extends YamlPathableConfig {
    public static final ConfigPath<String> SERVER_IP = Paths.stringPath("server.ip", "0.0.0.0");
    public static final ConfigPath<Integer> SERVER_PORT = Paths.integerPath("server.port", 25565);
    public static final ConfigPath<ProxyType> SERVER_TYPE = new AdvancedConfigPath<String, ProxyType>("server.type", ProxyType.NONE) {
        @Override
        public @Nullable String getFromConfig(@NotNull Config config) {
            return Objects.toString(config.get(getPath()), null);
        }

        @Override
        public @Nullable ProxyType convert(@NotNull String rawValue) {
            try {
                return ProxyType.valueOf(rawValue.toUpperCase());
            } catch (Exception e) {
                return null;
            }
        }

        @Override
        public @NotNull String convertToRaw(@NotNull ProxyType value) {
            return value.name();
        }
    };
    public static final ConfigPath<String> SERVER_VELOCITY_SECRET = Paths.stringPath("server.velocity-secret", "");
    public static final ConfigPath<String> SERVER_BRAND_NAME = Paths.stringPath("server.brand-name", FlexEGames.class.getSimpleName());
    public static final ConfigPath<Integer> SERVER_COMPRESSION_THRESHOLD = Paths.integerPath("server.compression-threshold", 0);
    public static final ConfigPath<Boolean> SERVER_SHOW_PLAYERS = Paths.booleanPath("server.show-players", true);
    public static final ConfigPath<List<Component>> SERVER_MOTD = new ComponentListPath("server.motd", Arrays.asList(
            Component.text("FlexEGames").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD),
            Component.text("A Minecraft server").color(NamedTextColor.GRAY)
    ));
    public static final ConfigPath<Integer> ARENA_PERIOD = Paths.integerPath("arena.period", 0);
    public static final ConfigPath<Boolean> ARENA_ASYNC = Paths.booleanPath("arena.async", true);
    public static final ConfigPath<Integer> ARENA_AMOUNT_PER_PLAYER = Paths.integerPath("arena.amount-per-player", -1);
    public static final ConfigPath<Map<String, Component>> CUSTOM_PLACEHOLDERS = new ComponentMapPath("custom-placeholders", Collections.emptyMap());
    public static final ConfigPath<List<Permission>> CONSOLE_PERMISSIONS = Paths.sticky(new PermissionListPath("console.permissions", Collections.emptyList()));
    public static final ConfigPath<List<Permission>> PLAYER_DEFAULT_PERMISSIONS = Paths.sticky(new PermissionListPath("player.default-permissions", Collections.emptyList()));
    public static final ConfigPath<Map<String, List<Permission>>> PLAYER_PERMISSIONS = Paths.sticky(new PermissionListMapPath("player.permissions", Collections.emptyMap()));

    public MainConfig() {
        super(new File("config.yml"));
    }

    public static List<Permission> getPlayerPermissions(String username) {
        var permissions = new ArrayList<>(PLAYER_DEFAULT_PERMISSIONS.getValue());
        Optional.ofNullable(PLAYER_PERMISSIONS.getValue().get(username)).ifPresent(permissions::addAll);
        return permissions;
    }
}
