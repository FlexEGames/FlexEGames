package me.hsgamer.flexegames.feature;

import lombok.Setter;
import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.GameDescriptionConfig;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.item.ItemStack;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * The feature to show the description of the {@link Game}
 */
@ExtensionMethod({ItemUtil.class})
public class DescriptionFeature extends ArenaFeature<DescriptionFeature.ArenaDescriptionFeature> {
    private final Game game;
    /**
     * The function to provide the replacement map of an {@link Arena}
     */
    @Setter
    private Function<Arena, Map<String, Supplier<ComponentLike>>> replacementsFunction = a -> Collections.emptyMap();

    public DescriptionFeature(Game game) {
        this.game = game;
    }

    private GameDescriptionConfig getConfig() {
        return game.getFeature(ConfigFeature.class).getConfig(GameDescriptionConfig.class, true);
    }

    /**
     * Get the display name of the {@link Game}
     *
     * @return the display name
     */
    public Component getDisplayName() {
        return getConfig().getDisplayName();
    }

    /**
     * Get the description of the {@link Game}
     *
     * @return the description
     */
    public List<Component> getDescription() {
        return getConfig().getDescription();
    }

    /**
     * Get the display item of the {@link Game}
     *
     * @return the display item
     */
    public ItemStack getDisplayItem() {
        return ItemBuilder.buildItem(getConfig().getDisplayItem())
                .withDisplayName(getDisplayName())
                .withLore(getDescription())
                .stripItalics();
    }

    @Override
    protected ArenaDescriptionFeature createFeature(Arena arena) {
        return new ArenaDescriptionFeature(arena);
    }

    /**
     * The description feature for each {@link Arena}
     */
    public class ArenaDescriptionFeature implements Feature {
        private final Arena arena;

        public ArenaDescriptionFeature(Arena arena) {
            this.arena = arena;
        }

        /**
         * Get the replacement map of the {@link Arena}
         *
         * @return the replacement map
         */
        public Map<String, Supplier<ComponentLike>> getReplacements() {
            JoinFeature.ArenaJoinFeature joinFeature = arena.getArenaFeature(JoinFeature.class);
            Map<String, Supplier<ComponentLike>> replacements = new HashMap<>();
            replacements.put("players", () -> Component.text(Integer.toString(joinFeature.getPlayerCount())));
            replacements.put("max-players", () -> Component.text(Integer.toString(joinFeature.getMaxPlayers())));
            replacements.put("state", () -> arena.getStateInstance()
                    .map(state -> {
                        if (state instanceof ComponentGameState componentGameState) {
                            return componentGameState.getDisplayNameAsComponent();
                        }
                        return Component.text(state.getDisplayName());
                    })
                    .orElse(Component.empty())
            );
            replacements.put("game", DescriptionFeature.this::getDisplayName);
            replacements.put("owner", () -> arena.getArenaFeature(OwnerFeature.class).getDisplayOwner());
            replacements.put("name", () -> Component.text(arena.getName()));
            replacements.putAll(replacementsFunction.apply(arena));
            return replacements;
        }

        /**
         * Get the display name of the {@link Arena}
         *
         * @return the display name
         */
        public ItemStack getDisplayItem() {
            return ItemBuilder.buildItem(getConfig().getArenaDisplayItem(), getReplacements()).stripItalics();
        }
    }
}
