package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.FlexEGames;
import me.hsgamer.flexegames.config.converter.*;
import me.hsgamer.flexegames.util.ProxyType;
import me.hsgamer.hscore.config.annotation.ConfigPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.permission.Permission;

import java.util.*;

/**
 * The main config
 */
public interface MainConfig {
    @ConfigPath("server.ip")
    default String getServerIp() {
        return "0.0.0.0";
    }

    @ConfigPath("server.port")
    default int getServerPort() {
        return 25565;
    }

    @ConfigPath(value = "server.proxy-type", converter = ProxyTypeConverter.class)
    default ProxyType getProxyType() {
        return ProxyType.NONE;
    }

    @ConfigPath("server.lan")
    default boolean isLANsupported() {
        return false;
    }

    @ConfigPath("server.velocity-secret")
    default String getVelocitySecret() {
        return "";
    }

    @ConfigPath("server.brand")
    default String getServerBrand() {
        return FlexEGames.class.getSimpleName();
    }

    @ConfigPath("server.compression-threshold")
    default int getCompressionThreshold() {
        return 0;
    }

    @ConfigPath("server.show-players")
    default boolean isShowPlayers() {
        return true;
    }

    @ConfigPath(value = "server.motd", converter = ComponentListConverter.class)
    default List<Component> getServerMOTD() {
        return Arrays.asList(
                Component.text("FlexEGames").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD),
                Component.text("A Minecraft server").color(NamedTextColor.GRAY)
        );
    }

    @ConfigPath("server.favicon")
    default String getServerFavicon() {
        return "";
    }

    @ConfigPath("scheduler.size")
    default int getSchedulerSize() {
        return Runtime.getRuntime().availableProcessors();
    }

    @ConfigPath(value = "custom-placeholders", converter = ComponentMapConverter.class)
    default Map<String, Component> getCustomPlaceholders() {
        return Map.of(
                "server-name", Component.text("FlexEGames").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD),
                "server-ip", Component.text("example.com").color(NamedTextColor.GRAY)
        );
    }

    @ConfigPath(value = "permission.console", converter = PermissionListConverter.class)
    default List<Permission> getConsolePermissions() {
        return Collections.emptyList();
    }

    @ConfigPath(value = "permission.player-default", converter = PermissionListConverter.class)
    default List<Permission> getPlayerDefaultPermissions() {
        return Collections.emptyList();
    }

    @ConfigPath(value = "permission.player", converter = PermissionListMapConverter.class)
    default Map<String, List<Permission>> getPlayerPermissions() {
        return Collections.emptyMap();
    }

    default List<Permission> getPlayerPermissions(String name) {
        var permissions = new ArrayList<>(getPlayerDefaultPermissions());
        Optional.ofNullable(getPlayerPermissions().get(name)).ifPresent(permissions::addAll);
        return permissions;
    }
}
