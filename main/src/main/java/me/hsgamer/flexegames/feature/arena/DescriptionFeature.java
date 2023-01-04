package me.hsgamer.flexegames.feature.arena;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.extra.DisplayName;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.item.ItemStack;

import java.util.Map;
import java.util.function.Supplier;

/**
 * The feature to show the description of the {@link Arena}
 */
public interface DescriptionFeature extends Feature {
    /**
     * Get the default replacement map of the {@link Arena}
     *
     * @param arena the {@link Arena}
     * @return the default replacement map
     */
    static Map<String, Supplier<ComponentLike>> getDefaultReplacements(Arena arena) {
        JoinFeature joinFeature = arena.getFeature(JoinFeature.class);
        return Map.of(
                "players", () -> Component.text(Integer.toString(joinFeature.getPlayerCount())),
                "max-players", () -> Component.text(Integer.toString(joinFeature.getMaxPlayers())),
                "state", () -> arena.getCurrentStateInstance()
                        .map(state -> {
                            if (state instanceof ComponentDisplayName componentDisplayName) {
                                return componentDisplayName.getDisplayNameAsComponent();
                            } else if (state instanceof DisplayName displayName) {
                                return Component.text(displayName.getDisplayName());
                            }
                            return null;
                        })
                        .orElse(Component.empty()),
                "game", () -> arena.getFeature(GameFeature.class).game().getDisplayName(),
                "name", () -> Component.text(arena.getName())
        );
    }

    /**
     * Get the replacement map of the {@link Arena}
     *
     * @return the replacement map
     */
    Map<String, Supplier<ComponentLike>> getReplacements();

    /**
     * Get the display name of the {@link Arena}
     *
     * @return the display name
     */
    ItemStack getDisplayItem();
}
