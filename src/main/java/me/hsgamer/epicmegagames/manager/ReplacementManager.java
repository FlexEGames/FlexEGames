package me.hsgamer.epicmegagames.manager;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.entity.Player;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
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
        return component;
    }

    public static Component replaceGlobal(Component component) {
        return replace(component, globalMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().get())));
    }

    public static Component replacePlayer(Component component, Player player) {
        return replace(component, playerMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().apply(player))));
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {
        private final List<UnaryOperator<Component>> replacements;

        private Builder() {
            replacements = new LinkedList<>();
        }

        public Builder replacePlayer(Player player) {
            replacements.add(component -> ReplacementManager.replacePlayer(component, player));
            return this;
        }

        public Builder replaceGlobal() {
            replacements.add(ReplacementManager::replaceGlobal);
            return this;
        }

        public Builder replace(Map<String, ComponentLike> map) {
            replacements.add(component -> ReplacementManager.replace(component, map));
            return this;
        }

        public Component build(Component component) {
            for (UnaryOperator<Component> replacement : replacements) {
                component = replacement.apply(component);
            }
            return component;
        }
    }
}