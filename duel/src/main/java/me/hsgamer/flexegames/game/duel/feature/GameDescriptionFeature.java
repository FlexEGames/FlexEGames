package me.hsgamer.flexegames.game.duel.feature;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.flexegames.util.TimeUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

public class GameDescriptionFeature implements DescriptionFeature {
    private final Arena arena;
    private final DuelExtension duelExtension;

    public GameDescriptionFeature(Arena arena, DuelExtension duelExtension) {
        this.arena = arena;
        this.duelExtension = duelExtension;
    }

    @Override
    public Map<String, Supplier<ComponentLike>> getReplacements() {
        Map<String, Supplier<ComponentLike>> map = new HashMap<>(DescriptionFeature.getDefaultReplacements(arena));
        map.putAll(Map.of(
                "time", () -> Component.text(TimeUtil.format(arena.getFeature(TimerFeature.class).getDuration())),
                "winner", () -> Optional.ofNullable(arena.getFeature(WinnerFeature.class).getWinner()).map(Player::getName).orElse(Component.empty()),
                "alive", () -> Component.text(Integer.toString(arena.getFeature(InstanceFeature.class).getAlivePlayers().size())),
                "world", () -> arena.getFeature(InstanceFeature.class).getDuelWorld().getDisplayName(),
                "kit", () -> arena.getFeature(InstanceFeature.class).getDuelKit().getDisplayName()
        ));
        return map;
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemUtil.stripItalics(
                ItemBuilder.buildItem(duelExtension.getMessageConfig().getArenaDisplayItem(), getReplacements())
        );
    }
}
