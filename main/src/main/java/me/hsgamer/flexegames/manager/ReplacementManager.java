package me.hsgamer.flexegames.manager;

import lombok.experimental.UtilityClass;
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
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * The replacement manager
 */
@UtilityClass
public final class ReplacementManager {
    private static final Pattern matchPattern = Pattern.compile("%([^%]+)%");
    private static final Map<String, Supplier<ComponentLike>> globalMap = new HashMap<>();
    private static final Map<String, Function<Player, ComponentLike>> playerMap = new HashMap<>();

    /**
     * Register a global replacement
     *
     * @param key      the key
     * @param supplier the supplier of the replacement
     */
    public static void addGlobalReplacement(String key, Supplier<ComponentLike> supplier) {
        globalMap.put(key, supplier);
    }

    /**
     * Register a player replacement
     *
     * @param key      the key
     * @param function the function to provide the replacement for the player
     */
    public static void addPlayerReplacement(String key, Function<Player, ComponentLike> function) {
        playerMap.put(key, function);
    }

    /**
     * Replace the component
     *
     * @param component the component
     * @param map       the replacement map
     * @return the replaced component
     */
    public static Component replace(Component component, Map<String, Supplier<ComponentLike>> map) {
        return component.replaceText(builder -> builder.match(matchPattern).replacement((matchResult, builder1) -> {
            String key = matchResult.group(1);
            Supplier<ComponentLike> supplier = map.get(key);
            if (supplier != null) {
                return supplier.get();
            }
            return Component.text("%" + key + "%");
        }));
    }

    /**
     * Replace the component with the global replacement map
     *
     * @param component the component
     * @return the replaced component
     */
    public static Component replaceGlobal(Component component) {
        return replace(component, globalMap);
    }

    /**
     * Replace the component with the player replacement map
     *
     * @param component the component
     * @param player    the player
     * @return the replaced component
     */
    public static Component replacePlayer(Component component, Player player) {
        return replace(component, playerMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> () -> entry.getValue().apply(player))));
    }

    /**
     * Convert the component map to a replacement map
     *
     * @param map the component map
     * @return the replacement map
     */
    public static Map<String, Supplier<ComponentLike>> toSupplierMap(Map<String, ComponentLike> map) {
        return map.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry::getValue));
    }

    /**
     * Create a builder to replace the component
     *
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * The builder to replace the component using the Builder pattern
     */
    public static final class Builder {
        private final List<UnaryOperator<Component>> replacements;

        private Builder() {
            replacements = new LinkedList<>();
        }

        /**
         * Add player replacements
         *
         * @param player the player
         * @return the builder
         */
        public Builder replacePlayer(Player player) {
            replacements.add(component -> ReplacementManager.replacePlayer(component, player));
            return this;
        }

        /**
         * Add global replacements
         *
         * @return the builder
         */
        public Builder replaceGlobal() {
            replacements.add(ReplacementManager::replaceGlobal);
            return this;
        }

        /**
         * Add replacements
         *
         * @param map the replacement map
         * @return the builder
         */
        public Builder replace(Map<String, Supplier<ComponentLike>> map) {
            replacements.add(component -> ReplacementManager.replace(component, map));
            return this;
        }

        /**
         * Replace the component
         *
         * @param component the component
         * @return the replaced component
         */
        public Component build(Component component) {
            for (UnaryOperator<Component> replacement : replacements) {
                component = replacement.apply(component);
            }
            return component;
        }
    }
}
