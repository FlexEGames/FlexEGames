package me.hsgamer.flexegames;

import com.google.gson.reflect.TypeToken;
import lombok.Getter;
import me.hsgamer.flexegames.config.converter.*;
import me.hsgamer.flexegames.util.ProxyType;
import me.hsgamer.hscore.config.annotation.converter.manager.DefaultConverterManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.permission.Permission;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * The main class of FlexEGames
 */
public class FlexEGames {
    /**
     * The instance of the game server
     */
    @Getter
    private static GameServer gameServer;

    static {
        DefaultConverterManager.registerConverter(Component.class, new ComponentConverter());
        DefaultConverterManager.registerConverter(new TypeToken<List<Component>>() {
        }.getType(), new ComponentListConverter());
        DefaultConverterManager.registerConverter(new TypeToken<Map<String, Component>>() {
        }.getType(), new ComponentMapConverter());
        DefaultConverterManager.registerConverter(new TypeToken<List<Map<String, Object>>>() {
        }.getType(), new MapListConverter());
        DefaultConverterManager.registerConverter(new TypeToken<Map<String, Object>>() {
        }.getType(), new StringObjectMapConverter());
        DefaultConverterManager.registerConverter(new TypeToken<Map<Number, Map<String, Object>>>() {
        }.getType(), new NumberObjectMapConverter());
        DefaultConverterManager.registerConverter(new TypeToken<List<Permission>>() {
        }.getType(), new PermissionListConverter());
        DefaultConverterManager.registerConverter(new TypeToken<Map<String, List<Permission>>>() {
        }.getType(), new PermissionListMapConverter());
        DefaultConverterManager.registerConverter(Pos.class, new PosConverter());
        DefaultConverterManager.registerConverter(new TypeToken<List<Pos>>() {
        }.getType(), new PosListConverter());
        DefaultConverterManager.registerConverter(ProxyType.class, new EnumConverter<>(ProxyType.class));
        DefaultConverterManager.registerConverter(UUID.class, new UuidConverter());
        DefaultConverterManager.registerConverter(new TypeToken<List<UUID>>() {
        }.getType(), new UuidListConverter());
    }

    public static void main(String[] args) {
        gameServer = new GameServer(args);
        gameServer.start();
    }
}
