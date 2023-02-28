package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.item.ItemStack;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public class PveGame implements Game {
    public static final int SPAWN_RADIUS = 10;
    public static final int HEIGHT = 16;
    private final PveExtension pveExtension;

    public PveGame(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
    }

    @Override
    public boolean isConfigured() {
        return true;
    }

    @Override
    public Component getDisplayName() {
        return pveExtension.getMessageConfig().getDisplayName();
    }

    @Override
    public List<Component> getDescription() {
        return pveExtension.getMessageConfig().getDescription();
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemUtil.stripItalics(
                ItemBuilder.buildItem(pveExtension.getMessageConfig().getDisplayItem())
                        .withDisplayName(getDisplayName())
                        .withLore(getDescription())
        );
    }

    @Override
    public CompletableFuture<GamePropertyMap> editProperty(Player player, GamePropertyMap gamePropertyMap) {
        return CompletableFuture.completedFuture(gamePropertyMap); // TODO
    }

    @Override
    public Arena create(String name, GamePropertyMap gamePropertyMap, ArenaManager arenaManager, UUID owner) {
        return new PveArena(pveExtension, name, gamePropertyMap, this, arenaManager, owner);
    }
}
