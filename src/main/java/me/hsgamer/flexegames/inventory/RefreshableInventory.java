package me.hsgamer.flexegames.inventory;

import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.inventory.InventoryCloseEvent;
import net.minestom.server.event.inventory.InventoryOpenEvent;
import net.minestom.server.event.trait.InventoryEvent;
import net.minestom.server.inventory.Inventory;
import net.minestom.server.inventory.InventoryType;
import net.minestom.server.item.ItemStack;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public class RefreshableInventory extends Inventory {
    private final ButtonMap buttonMap;
    private final AtomicReference<Map<Integer, Button>> currentSlotMap = new AtomicReference<>(Collections.emptyMap());
    private final EventNode<InventoryEvent> eventNode;
    private final RefreshHandler refreshHandler;
    private final AtomicBoolean firstTime = new AtomicBoolean(true);

    private RefreshableInventory(
            @NotNull InventoryType inventoryType,
            @NotNull Component title,
            @NotNull ButtonMap buttonMap,
            boolean cancelClickByDefault,
            @NotNull OpenHandler openHandler,
            @NotNull CloseHandler closeHandler,
            @NotNull RefreshHandler refreshHandler) {
        super(inventoryType, title);
        this.buttonMap = buttonMap;
        this.refreshHandler = refreshHandler;
        this.eventNode = EventNode.event("inventory-" + UUID.randomUUID(), EventFilter.INVENTORY, event -> Objects.equals(event.getInventory(), this));

        addInventoryCondition((player, slot, clickType, inventoryConditionResult) -> {
            var button = currentSlotMap.get().get(slot);
            if (button == null) {
                if (cancelClickByDefault) {
                    inventoryConditionResult.setCancel(true);
                }
            } else {
                if ((!inventoryConditionResult.isCancel() || !button.ignoreCancelled()) && button.getClickConsumer().onClick(player, clickType, inventoryConditionResult)) {
                    refresh();
                }
            }
        });
        eventNode
                .addListener(InventoryOpenEvent.class, openEvent -> {
                    if (!openHandler.test(openEvent.getPlayer())) openEvent.setCancelled(true);
                })
                .addListener(InventoryCloseEvent.class, closeEvent -> {
                    if (!closeHandler.test(closeEvent.getPlayer())) closeEvent.setNewInventory(this);
                });
        MinecraftServer.getGlobalEventHandler().addChild(eventNode);

        refresh();
    }

    public static Builder builder() {
        return new Builder();
    }

    public EventNode<InventoryEvent> getEventNode() {
        return eventNode;
    }

    public void refresh() {
        boolean canRefresh = refreshHandler.canRefresh(this, firstTime.getAndSet(false));
        if (firstTime.get()) {
            firstTime.set(false);
        }
        if (!canRefresh) return;
        var slotMap = buttonMap.getButtons(this);
        currentSlotMap.set(slotMap);
        for (int i = 0; i < getSize(); i++) {
            var button = slotMap.get(i);
            if (button == null) {
                setItemStack(i, ItemStack.AIR);
            } else {
                setItemStack(i, button.getItem());
            }
        }
    }

    public RefreshableInventory autoRefresh(TaskSchedule taskSchedule) {
        AtomicReference<Task> task = new AtomicReference<>();
        getEventNode()
                .addListener(InventoryOpenEvent.class, event -> {
                    Optional.ofNullable(task.get()).ifPresent(Task::cancel);
                    task.set(MinecraftServer.getSchedulerManager().buildTask(this::refresh).delay(taskSchedule).repeat(taskSchedule).schedule());
                })
                .addListener(InventoryCloseEvent.class, event -> {
                    if (getViewers().size() > 1) return;
                    Optional.ofNullable(task.get()).ifPresent(Task::cancel);
                });
        return this;
    }

    public RefreshableInventory unregisterWhenClosed() {
        getEventNode().addListener(InventoryCloseEvent.class, event -> {
            if (getViewers().size() > 1) return;
            MinecraftServer.getGlobalEventHandler().removeChild(eventNode);
        });
        return this;
    }

    public static class Builder {
        private InventoryType inventoryType;
        private Component title;
        private ButtonMap buttonMap;
        private boolean cancelClickByDefault;
        private OpenHandler openHandler;
        private CloseHandler closeHandler;
        private RefreshHandler refreshHandler;

        private Builder() {
            cancelClickByDefault = true;
            openHandler = player -> true;
            closeHandler = player -> true;
            refreshHandler = (inv, b) -> true;
        }

        public Builder setInventoryType(@NotNull InventoryType inventoryType) {
            this.inventoryType = inventoryType;
            return this;
        }

        public Builder setTitle(@NotNull Component title) {
            this.title = title;
            return this;
        }

        public Builder setButtonMap(@NotNull ButtonMap buttonMap) {
            this.buttonMap = buttonMap;
            return this;
        }

        public Builder setCancelClickByDefault(boolean cancelClickByDefault) {
            this.cancelClickByDefault = cancelClickByDefault;
            return this;
        }

        public Builder setOpenHandler(@NotNull OpenHandler openHandler) {
            this.openHandler = openHandler;
            return this;
        }

        public Builder setCloseHandler(@NotNull CloseHandler closeHandler) {
            this.closeHandler = closeHandler;
            return this;
        }

        public Builder setRefreshHandler(@NotNull RefreshHandler refreshHandler) {
            this.refreshHandler = refreshHandler;
            return this;
        }

        public RefreshableInventory build() {
            if (inventoryType == null) {
                throw new IllegalStateException("inventoryType is null");
            }
            if (title == null) {
                throw new IllegalStateException("title is null");
            }
            if (buttonMap == null) {
                throw new IllegalStateException("buttonMap is null");
            }
            return new RefreshableInventory(
                    inventoryType,
                    title,
                    buttonMap,
                    cancelClickByDefault,
                    openHandler,
                    closeHandler,
                    refreshHandler);
        }
    }
}
