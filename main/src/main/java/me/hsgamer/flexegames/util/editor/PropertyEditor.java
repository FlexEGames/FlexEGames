package me.hsgamer.flexegames.util.editor;

import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.hscore.minecraft.gui.advanced.AdvancedButtonMap;
import me.hsgamer.hscore.minecraft.gui.event.CloseEvent;
import me.hsgamer.hscore.minecraft.gui.mask.MaskUtils;
import me.hsgamer.hscore.minecraft.gui.mask.impl.ButtonMapMask;
import me.hsgamer.hscore.minecraft.gui.mask.impl.StaticButtonPaginatedMask;
import me.hsgamer.hscore.minestom.gui.MinestomGUIHolder;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.inventory.InventoryType;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

public abstract class PropertyEditor extends MinestomGUIHolder {
    private final Map<UUID, GamePropertyMapFuture> propertyMapFutureMap = new ConcurrentHashMap<>();

    @Override
    public void init() {
        setInventoryType(InventoryType.CHEST_3_ROW);
        setRemoveDisplayOnClose(true);

        AdvancedButtonMap buttonMap = new AdvancedButtonMap();
        setButtonMap(buttonMap);

        StaticButtonPaginatedMask valueMask = new StaticButtonPaginatedMask("value", MaskUtils.generateAreaSlots(0, 0, 8, 1).boxed().toList()).addButton(getValueButtons());
        ButtonMapMask actionMask = getActionMask(valueMask);
        buttonMap.addMask(valueMask);
        buttonMap.addMask(actionMask);

        super.init();
    }

    protected abstract ButtonMapMask getActionMask(StaticButtonPaginatedMask valueMask);

    protected ItemStack getDummyItem() {
        return ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty());
    }

    protected abstract ItemStack getCompleteItem();

    protected abstract List<PropertyValueButton<?>> getValueButtons();

    GamePropertyMap getPropertyMap(UUID uuid) {
        return Optional.ofNullable(propertyMapFutureMap.get(uuid))
                .map(GamePropertyMapFuture::current)
                .orElse(null);
    }

    protected void complete(UUID uuid) {
        Optional.ofNullable(propertyMapFutureMap.get(uuid))
                .ifPresent(GamePropertyMapFuture::complete);
    }

    @Override
    protected void onClose(@NotNull CloseEvent event) {
        Optional.ofNullable(propertyMapFutureMap.remove(event.getViewerID()))
                .filter(future -> !future.future().isDone())
                .ifPresent(GamePropertyMapFuture::cancel);
    }

    @Override
    public void stop() {
        super.stop();
        propertyMapFutureMap.values().forEach(GamePropertyMapFuture::cancel);
        propertyMapFutureMap.clear();
    }

    public CompletableFuture<GamePropertyMap> open(Player player, GamePropertyMap propertyMap) {
        CompletableFuture<GamePropertyMap> future = new CompletableFuture<>();
        var oldFuture = propertyMapFutureMap.put(player.getUuid(), new GamePropertyMapFuture(propertyMap, future));
        if (oldFuture != null) {
            oldFuture.cancel();
        }
        createDisplay(player.getUuid()).open();
        return future;
    }

    private record GamePropertyMapFuture(GamePropertyMap current, CompletableFuture<GamePropertyMap> future) {
        private void complete() {
            future.complete(current);
        }

        private void cancel() {
            future.cancel(true);
        }
    }
}
