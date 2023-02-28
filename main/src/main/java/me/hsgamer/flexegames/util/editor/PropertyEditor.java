package me.hsgamer.flexegames.util.editor;

import me.hsgamer.flexegames.api.property.GamePropertyMap;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.hscore.minecraft.gui.GUIDisplay;
import me.hsgamer.hscore.minecraft.gui.advanced.AdvancedButtonMap;
import me.hsgamer.hscore.minecraft.gui.button.Button;
import me.hsgamer.hscore.minecraft.gui.button.impl.DummyButton;
import me.hsgamer.hscore.minecraft.gui.button.impl.SimpleButton;
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

        Button nextPageButton = new SimpleButton(ItemUtil.asMinestomItem(getNextPageItem()), event -> {
            valueMask.nextPage(event.getViewerID());
            getDisplay(event.getViewerID()).ifPresent(GUIDisplay::update);
        });
        Button previousPageButton = new SimpleButton(ItemUtil.asMinestomItem(getPreviousPageItem()), event -> {
            valueMask.previousPage(event.getViewerID());
            getDisplay(event.getViewerID()).ifPresent(GUIDisplay::update);
        });
        Button completeButton = new SimpleButton(ItemUtil.asMinestomItem(getCompleteItem()), event -> complete(event.getViewerID()));
        Button dummyButton = new DummyButton(ItemUtil.asMinestomItem(getDummyItem()));
        ButtonMapMask actionMask = new ButtonMapMask("action")
                .addButton(previousPageButton, 18)
                .addButton(nextPageButton, 19)
                .addButton(dummyButton, 20, 21, 22, 23, 24, 25)
                .addButton(completeButton, 26);

        buttonMap.addMask(valueMask);
        buttonMap.addMask(actionMask);

        super.init();
    }

    protected abstract ItemStack getNextPageItem();

    protected abstract ItemStack getPreviousPageItem();

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

    private void complete(UUID uuid) {
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
        propertyMapFutureMap.put(player.getUuid(), new GamePropertyMapFuture(propertyMap, future));
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
