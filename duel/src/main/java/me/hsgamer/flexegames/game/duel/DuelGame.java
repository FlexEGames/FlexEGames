package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.api.property.PropertyMap;
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

public class DuelGame implements Game {
    private final DuelExtension duelExtension;

    public DuelGame(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public Component getDisplayName() {
        return duelExtension.getMessageConfig().getDisplayName();
    }

    @Override
    public List<Component> getDescription() {
        return duelExtension.getMessageConfig().getDescription();
    }

    @Override
    public ItemStack getDisplayItem() {
        return ItemUtil.stripItalics(
                ItemBuilder.buildItem(duelExtension.getMessageConfig().getDisplayItem())
                        .withDisplayName(getDisplayName())
                        .withLore(getDescription())
        );
    }

    @Override
    public CompletableFuture<PropertyMap> editProperty(Player player, PropertyMap propertyMap) {
        return duelExtension.getPropertyEditor().open(player, propertyMap);
    }

    @Override
    public Arena create(String name, PropertyMap propertyMap, ArenaManager arenaManager, UUID owner) {
        return new DuelArena(duelExtension, name, propertyMap, this, arenaManager, owner);
    }
}
