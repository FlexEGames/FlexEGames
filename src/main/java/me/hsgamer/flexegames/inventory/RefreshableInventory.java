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
import java.util.function.Supplier;

public class RefreshableInventory extends Inventory {
    private final ButtonMap buttonMap;
    private final AtomicReference<Map<Integer, Button>> currentSlotMap = new AtomicReference<>(Collections.emptyMap());
    private final EventNode<InventoryEvent> eventNode;
    private final RefreshHandler refreshHandler;
    private final AtomicBoolean firstTime = new AtomicBoolean(true);
    private final boolean unregisterOnClose;

    private RefreshableInventory(
            @NotNull InventoryType inventoryType,
            @NotNull Component title,
            @NotNull ButtonMap buttonMap,
            boolean cancelClickByDefault,
            boolean unregisterOnClose,
            @NotNull OpenHandler openHandler,
            @NotNull CloseHandler closeHandler,
            @NotNull RefreshHandler refreshHandler,
            @NotNull TaskSchedule updateSchedule
    ) {
        super(inventoryType, title);
        this.buttonMap = buttonMap;
        this.unregisterOnClose = unregisterOnClose;
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
        autoRefresh(updateSchedule);
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

    private void autoRefresh(TaskSchedule taskSchedule) {
        AtomicReference<Task> task = new AtomicReference<>();
        getEventNode()
                .addListener(InventoryOpenEvent.class, event -> {
                    Optional.ofNullable(task.get()).ifPresent(Task::cancel);
                    task.set(MinecraftServer.getSchedulerManager().submitTask(new Supplier<>() {
                        private boolean first = true;

                        @Override
                        public TaskSchedule get() {
                            if (first) {
                                first = false;
                                return taskSchedule;
                            }
                            if (getViewers().isEmpty()) {
                                if (unregisterOnClose) {
                                    unregister();
                                }
                                return TaskSchedule.stop();
                            }
                            refresh();
                            return taskSchedule;
                        }
                    }));
                });
    }

    private void unregister() {
        MinecraftServer.getGlobalEventHandler().removeChild(eventNode);
    }

    public static class Builder {
        private InventoryType inventoryType;
        private Component title;
        private ButtonMap buttonMap;
        private boolean cancelClickByDefault;
        private boolean unregisterOnClose;
        private OpenHandler openHandler;
        private CloseHandler closeHandler;
        private RefreshHandler refreshHandler;
        private TaskSchedule updateSchedule;

        private Builder() {
            cancelClickByDefault = true;
            unregisterOnClose = false;
            openHandler = player -> true;
            closeHandler = player -> true;
            refreshHandler = (inv, b) -> true;
            updateSchedule = TaskSchedule.tick(20);
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

        public Builder setUnregisterOnClose(boolean unregisterOnClose) {
            this.unregisterOnClose = unregisterOnClose;
            return this;
        }

        public Builder setUpdateSchedule(@NotNull TaskSchedule updateSchedule) {
            this.updateSchedule = updateSchedule;
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
                    unregisterOnClose,
                    openHandler,
                    closeHandler,
                    refreshHandler,
                    updateSchedule
            );
        }
    }
}
