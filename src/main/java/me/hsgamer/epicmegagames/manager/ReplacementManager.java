package me.hsgamer.epicmegagames.manager;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.entity.Player;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public final class ReplacementManager {
    private static final Map<String, Supplier<ComponentLike>> globalMap = new HashMap<>();
    private static final Map<String, Function<Player, ComponentLike>> playerMap = new HashMap<>();

    public static void addGlobalReplacement(String key, Supplier<ComponentLike> supplier) {
        globalMap.put(key, supplier);
    }

    public static void addPlayerReplacement(String key, Function<Player, ComponentLike> function) {
        playerMap.put(key, function);
    }

    public static Component replace(Component component, Map<String, ComponentLike> map) {
        for (Map.Entry<String, ComponentLike> entry : map.entrySet()) {
            component = component.replaceText(builder -> builder.match("%" + entry.getKey() + "%").replacement(entry.getValue()));
        }
        return replace(component);
    }

    public static Component replace(Component component) {
        return replace(component, globalMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().get())));
    }

    public static Component replace(Component component, Player player) {
        component = replace(component, playerMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().apply(player))));
        return replace(component);
    }

    public static Component replace(Component component, Player player, Map<String, ComponentLike> map) {
        component = replace(component, map);
        return replace(component, player);
    }
}
