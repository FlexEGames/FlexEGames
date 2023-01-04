package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.List;
import java.util.function.Function;

public class PveGame implements Game {
    public static final int SPAWN_RADIUS = 10;
    public static final int HEIGHT = 16;
    private final PveExtension pveExtension;
    private final PveGameConfig gameConfig;

    public PveGame(PveExtension pveExtension, PveGameConfig gameConfig) {
        this.pveExtension = pveExtension;
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
        return arenaManager -> new PveArena(pveExtension, this, name, arenaManager);
    }

    public PveGameConfig getGameConfig() {
        return gameConfig;
    }
}
