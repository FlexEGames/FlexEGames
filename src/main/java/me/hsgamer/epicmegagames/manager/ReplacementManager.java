package me.hsgamer.epicmegagames.manager;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.entity.Player;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

public class ReplacementManager {
    private final Map<String, Supplier<ComponentLike>> globalMap = new HashMap<>();
    private final Map<String, Function<Player, ComponentLike>> playerMap = new HashMap<>();

    public void addGlobalReplacement(String key, Supplier<ComponentLike> supplier) {
        globalMap.put(key, supplier);
    }

    public void addPlayerReplacement(String key, Function<Player, ComponentLike> function) {
        playerMap.put(key, function);
    }

    public Component replace(Component component) {
        for (Map.Entry<String, Supplier<ComponentLike>> entry : globalMap.entrySet()) {
            component = component.replaceText(builder -> builder.match("%" + entry.getKey() + "%").replacement(entry.getValue().get()));
        }
        return component;
    }

    public Component replace(Component component, Player player) {
        for (Map.Entry<String, Function<Player, ComponentLike>> entry : playerMap.entrySet()) {
            component = component.replaceText(builder -> builder.match("%" + entry.getKey() + "%").replacement(entry.getValue().apply(player)));
        }
        return replace(component);
    }

    public Component replace(Component component, Map<String, Supplier<ComponentLike>> map) {
        for (Map.Entry<String, Supplier<ComponentLike>> entry : map.entrySet()) {
            component = component.replaceText(builder -> builder.match("%" + entry.getKey() + "%").replacement(entry.getValue().get()));
        }
        return replace(component);
    }

    public Component replace(Component component, Player player, Map<String, Supplier<ComponentLike>> map) {
        for (Map.Entry<String, Supplier<ComponentLike>> entry : map.entrySet()) {
            component = component.replaceText(builder -> builder.match("%" + entry.getKey() + "%").replacement(entry.getValue().get()));
        }
        return replace(component, player);
    }
}
