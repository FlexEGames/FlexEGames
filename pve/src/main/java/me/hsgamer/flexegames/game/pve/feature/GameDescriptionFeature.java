package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.flexegames.util.TimeUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.minestom.server.item.ItemStack;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class GameDescriptionFeature implements DescriptionFeature {
    private final Arena arena;
    private final PveExtension pveExtension;

    public GameDescriptionFeature(Arena arena, PveExtension pveExtension) {
        this.arena = arena;
        this.pveExtension = pveExtension;
    }

    @Override
    public Map<String, Supplier<ComponentLike>> getReplacements() {
        Map<String, Supplier<ComponentLike>> map = new HashMap<>(DescriptionFeature.getDefaultReplacements(arena));
        map.putAll(Map.of(
                "time", () -> Component.text(TimeUtil.format(arena.getFeature(TimerFeature.class).getDuration())),
                "stage", () -> Component.text(Integer.toString(arena.getFeature(StageFeature.class).getStage())),
                "alive", () -> Component.text(Integer.toString(arena.getFeature(InstanceFeature.class).getAlivePlayers().size())),
                "mob", () -> Component.text(Integer.toString(arena.getFeature(MobGeneratorFeature.class).getMobCount())),
                "max-mob", () -> Component.text(Integer.toString(arena.getFeature(MobGeneratorFeature.class).getMaxMobCount())),
                "kit", () -> arena.getFeature(InstanceFeature.class).getGameKit().getDisplayName()
        ));
        return map;
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemUtil.stripItalics(
                ItemBuilder.buildItem(pveExtension.getMessageConfig().getArenaDisplayItem(), getReplacements())
        );
    }
}
