package me.hsgamer.flexegames.config;

import me.hsgamer.flexegames.FlexEGames;
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

    @ConfigPath("server.proxy-type")
    default ProxyType getProxyType() {
        return ProxyType.NONE;
    }

    @ConfigPath("server.lan")
    default boolean isLANsupported() {
        return false;
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

    @ConfigPath("server.motd")
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

    @ConfigPath("arena.period")
    default int getArenaPeriod() {
        return 0;
    }

    @ConfigPath("arena.async")
    default boolean isArenaAsync() {
        return false;
    }

    @ConfigPath("custom-placeholders")
    default Map<String, Component> getCustomPlaceholders() {
        return Map.of(
                "server-name", Component.text("FlexEGames").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD),
                "server-ip", Component.text("example.com").color(NamedTextColor.GRAY)
        );
    }

    @ConfigPath("permission.console")
    default List<Permission> getConsolePermissions() {
        return Collections.emptyList();
    }

    @ConfigPath("permission.player-default")
    default List<Permission> getPlayerDefaultPermissions() {
        return Collections.emptyList();
    }

    @ConfigPath("permission.player")
    default Map<String, List<Permission>> getPlayerPermissions() {
        return Collections.emptyMap();
    }

    default List<Permission> getPlayerPermissions(String name) {
        var permissions = new ArrayList<>(getPlayerDefaultPermissions());
        Optional.ofNullable(getPlayerPermissions().get(name)).ifPresent(permissions::addAll);
        return permissions;
    }
}
