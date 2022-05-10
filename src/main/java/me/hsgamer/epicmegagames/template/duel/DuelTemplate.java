package me.hsgamer.epicmegagames.template.duel;

import me.hsgamer.epicmegagames.api.ArenaGame;
import me.hsgamer.epicmegagames.api.Template;
import me.hsgamer.epicmegagames.builder.ItemBuilder;
import me.hsgamer.epicmegagames.config.path.*;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.ConfigPath;
import me.hsgamer.hscore.config.path.impl.Paths;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DuelTemplate implements Template {
    private static final ConfigPath<Component> displayNamePath = new ComponentPath("display-name", Component.text("Duel").color(NamedTextColor.YELLOW).decorate(TextDecoration.BOLD));
    private static final ConfigPath<List<Component>> descriptionPath = new ComponentListPath("description", Collections.singletonList(
            Component.text("Kill other players to win").color(NamedTextColor.WHITE)
    ));
    private static final ConfigPath<Map<String, Object>> displayItemPath = new MapPath("display-item", Map.of("material", Material.DIAMOND_SWORD.name()));
    private static final ConfigPath<Map<String, Object>> gameDisplayItemPath = new MapPath("game-display-item", Map.of(
            "material", Material.DIAMOND_SWORD.name(),
            "name", "%template%",
            "lore", List.of(
                    "&ePlayers: &f%players%/%max-players%",
                    "&eState: &f%state%",
                    "&eTime: &f%time%"
            )
    ));
    private static final ConfigPath<List<Pos>> posPath = new PosListPath("pos", List.of(
            new Pos(-2, 2, 0, -90, 0),
            new Pos(2, 2, 0, 90, 0),
            new Pos(0, 2, -2, 0, 0),
            new Pos(0, 2, 2, 180, 0)
    ));
    private static final ConfigPath<Pos> joinPosPath = new PosPath("join-pos", new Pos(0, 2, 0));
    private static final ConfigPath<Integer> maxHeightPath = Paths.integerPath("max-height", 2);
    private static final ConfigPath<Integer> waitingTimePath = Paths.integerPath("waiting-time", 60);
    private static final ConfigPath<Integer> endingTimePath = Paths.integerPath("ending-time", 5);
    private static final ConfigPath<Boolean> useLegacyPvpPath = Paths.booleanPath("use-legacy-pvp", false);
    private static final NumberObjectMapPath kitPath = new NumberObjectMapPath("kit", Collections.emptyMap());
    private static final ConfigPath<Double> borderDiameterPath = Paths.doublePath("border-diameter", 50.0);
    final Component displayName;
    final List<Component> description;
    final Map<String, Object> displayItem;
    final Map<String, Object> gameDisplayItem;
    final List<Pos> posList;
    final Pos joinPos;
    final int maxHeight;
    final int waitingTime;
    final int endingTime;
    final boolean useLegacyPvp;
    final Map<Integer, ItemStack> kit;
    final double borderDiameter;

    public DuelTemplate(Config config) {
        displayName = displayNamePath.getValue(config);
        description = descriptionPath.getValue(config);
        displayItem = displayItemPath.getValue(config);
        gameDisplayItem = gameDisplayItemPath.getValue(config);
        posList = posPath.getValue(config);
        joinPos = joinPosPath.getValue(config);
        maxHeight = maxHeightPath.getValue(config);
        waitingTime = waitingTimePath.getValue(config);
        endingTime = endingTimePath.getValue(config);
        useLegacyPvp = useLegacyPvpPath.getValue(config);
        kit = new HashMap<>();
        kitPath.getValue(config).forEach((key, value) -> kit.put(key.intValue(), ItemBuilder.buildItem(value)));
        borderDiameter = borderDiameterPath.getValue(config);
    }

    @Override
    public ArenaGame createGame(Arena arena) {
        return new DuelGame(this, arena);
    }

    @Override
    public Component getDisplayName() {
        return displayName;
    }

    @Override
    public List<Component> getDescription() {
        return description;
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemBuilder.buildItem(displayItem).withDisplayName(displayName).withLore(description);
    }
}
