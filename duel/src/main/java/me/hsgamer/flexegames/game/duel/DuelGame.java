package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.List;
import java.util.function.Function;

public class DuelGame implements Game {
    private final DuelExtension duelExtension;
    private final DuelGameConfig gameConfig;

    public DuelGame(DuelExtension duelExtension, DuelGameConfig gameConfig) {
        this.duelExtension = duelExtension;
        this.gameConfig = gameConfig;
    }

    @Override
    public boolean isConfigured() {
        return true;
    }

    @Override
    public Component getDisplayName() {
        return gameConfig.getDisplayName();
    }

    @Override
    public List<Component> getDescription() {
        return gameConfig.getDescription();
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemUtil.stripItalics(
                ItemBuilder.buildItem(gameConfig.getDisplayItem())
                        .withDisplayName(getDisplayName())
                        .withLore(getDescription())
        );
    }

    @Override
    public Function<ArenaManager, Arena> createArena(String name) {
        return arenaManager -> new DuelArena(duelExtension, this, name, arenaManager);
    }

    public DuelGameConfig getGameConfig() {
        return gameConfig;
    }
}
