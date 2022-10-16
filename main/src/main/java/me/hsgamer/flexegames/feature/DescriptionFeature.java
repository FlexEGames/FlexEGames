package me.hsgamer.flexegames.feature;

import lombok.Setter;
import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.game.GameDescriptionConfig;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.ToIntFunction;

@ExtensionMethod({ItemUtil.class})
public class DescriptionFeature extends ArenaFeature<DescriptionFeature.ArenaDescriptionFeature> {
    private final Game game;

    public DescriptionFeature(Game game) {
        this.game = game;
    }

    private GameDescriptionConfig getConfig() {
        return game.getFeature(ConfigFeature.class).getConfig(GameDescriptionConfig.class, true);
    }

    public Component getDisplayName() {
        return getConfig().getDisplayName();
    }

    public List<Component> getDescription() {
        return getConfig().getDescription();
    }

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

    public class ArenaDescriptionFeature implements Feature {
        private final Arena arena;
        @Setter
        private Function<Arena, Collection<Player>> playersSupplier = a -> Collections.emptyList();
        @Setter
        private ToIntFunction<Arena> maxPlayersSupplier = a -> 0;
        @Setter
        private Supplier<Map<String, Supplier<ComponentLike>>> replacementsSupplier = Collections::emptyMap;

        public ArenaDescriptionFeature(Arena arena) {
            this.arena = arena;
        }

        public ItemStack getDisplayItem() {
            Map<String, Supplier<ComponentLike>> replacements = new HashMap<>();
            replacements.put("players", () -> Component.text(playersSupplier.apply(arena).size()));
            replacements.put("max-players", () -> Component.text(maxPlayersSupplier.applyAsInt(arena)));
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
            replacements.putAll(replacementsSupplier.get());
            return ItemBuilder.buildItem(getConfig().getArenaDisplayItem(), replacements).stripItalics();
        }
    }
}
