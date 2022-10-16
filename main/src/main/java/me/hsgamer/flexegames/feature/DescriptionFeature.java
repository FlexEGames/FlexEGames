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
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.ToIntFunction;

@ExtensionMethod({ItemUtil.class})
public class DescriptionFeature extends ArenaFeature<DescriptionFeature.ArenaDescriptionFeature> {
    private final Game game;
    @Setter
    private Function<Arena, Collection<Player>> playersFunction = a -> Collections.emptyList();
    @Setter
    private ToIntFunction<Arena> maxPlayersFunction = a -> 0;
    @Setter
    private Function<Arena, Map<String, Supplier<ComponentLike>>> replacementsFunction = a -> Collections.emptyMap();

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

        public ArenaDescriptionFeature(Arena arena) {
            this.arena = arena;
        }

        public Collection<Player> getPlayers() {
            return playersFunction.apply(arena);
        }

        public int getPlayerCount() {
            return getPlayers().size();
        }

        public int getMaxPlayers() {
            return maxPlayersFunction.applyAsInt(arena);
        }

        public Map<String, Supplier<ComponentLike>> getReplacements() {
            Map<String, Supplier<ComponentLike>> replacements = new HashMap<>();
            replacements.put("players", () -> Component.text(Integer.toString(getPlayerCount())));
            replacements.put("max-players", () -> Component.text(Integer.toString(getMaxPlayers())));
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
            replacements.putAll(replacementsFunction.apply(arena));
            return replacements;
        }

        public ItemStack getDisplayItem() {
            return ItemBuilder.buildItem(getConfig().getArenaDisplayItem(), getReplacements()).stripItalics();
        }
    }
}
