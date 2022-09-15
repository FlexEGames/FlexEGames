package me.hsgamer.flexegames.hook;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.config.MainConfig;
import me.hsgamer.flexegames.util.AssetUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.server.ServerListPingEvent;
import net.minestom.server.ping.ResponseData;

import java.io.File;
import java.io.FileInputStream;
import java.util.Base64;
import java.util.Collection;
import java.util.List;

@UtilityClass
public final class ServerListHook {
    public static void hook(EventNode<Event> node) {
        Component motd = getMotd();
        String favicon = getFavicon();
        node.addListener(ServerListPingEvent.class, event -> {
            ResponseData responseData = event.getResponseData();
            Collection<Player> players = MinecraftServer.getConnectionManager().getOnlinePlayers();
            if (Boolean.TRUE.equals(MainConfig.SERVER_SHOW_PLAYERS.getValue())) {
                responseData.addEntries(players);
            }
            responseData.setOnline(players.size());
            responseData.setMaxPlayer(players.size() + 1);
            responseData.setDescription(motd);
            if (favicon != null) {
                responseData.setFavicon(favicon);
            }
        });
    }

    private static Component getMotd() {
        List<Component> motd = MainConfig.SERVER_MOTD.getValue();
        if (motd.isEmpty()) {
            return Component.empty();
        }
        Component component = motd.get(0);
        for (int i = 1; i < motd.size(); i++) {
            component = component.append(Component.newline()).append(motd.get(i));
        }
        return component;
    }

    private static String getFavicon() {
        String favicon = MainConfig.SERVER_FAVICON.getValue();
        if (favicon.isBlank()) {
            return null;
        }
        if (favicon.startsWith("data:image/png;base64,")) {
            return favicon;
        }
        File file = AssetUtil.getAssetFile(favicon);
        if (!file.exists()) {
            return null;
        }
        try (FileInputStream inputStream = new FileInputStream(file)) {
            byte[] bytes = inputStream.readAllBytes();
            return "data:image/png;base64," + Base64.getEncoder().encodeToString(bytes);
        } catch (Exception e) {
            MinecraftServer.LOGGER.error("Failed to read favicon", e);
            return null;
        }
    }
}
