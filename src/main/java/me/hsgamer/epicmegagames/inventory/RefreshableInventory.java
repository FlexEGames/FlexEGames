package me.hsgamer.epicmegagames.inventory;

import me.hsgamer.epicmegagames.util.TaskUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.event.inventory.InventoryCloseEvent;
import net.minestom.server.event.inventory.InventoryOpenEvent;
import net.minestom.server.inventory.Inventory;
import net.minestom.server.inventory.InventoryType;
import net.minestom.server.item.ItemStack;
import net.minestom.server.timer.Task;
import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

public class RefreshableInventory extends Inventory {
    private RefreshableInventory(
            @NotNull InventoryType inventoryType,
            @NotNull Component title,
            @NotNull ButtonMap buttonMap,
            int updateTick,
            boolean cancelClickByDefault,
            @NotNull OpenHandler openHandler,
            @NotNull CloseHandler closeHandler
    ) {
        super(inventoryType, title);

        final AtomicReference<Map<Integer, Button>> currentSlotMap = new AtomicReference<>(Collections.emptyMap());
        addInventoryCondition((player, slot, clickType, inventoryConditionResult) -> {
            Button button = currentSlotMap.get().get(slot);
            if (button == null) {
                if (cancelClickByDefault) {
                    inventoryConditionResult.setCancel(true);
                }
            } else {
                if (!inventoryConditionResult.isCancel() || !button.ignoreCancelled()) {
                    button.getClickConsumer().onClick(player, clickType, inventoryConditionResult);
                }
            }
        });

        final AtomicReference<Task> refreshTask = new AtomicReference<>();
        MinecraftServer.getGlobalEventHandler()
                .addListener(InventoryOpenEvent.class, openEvent -> {
                    if (!Objects.equals(openEvent.getInventory(), this)) return;
                    if (!openHandler.test(openEvent.getPlayer())) {
                        openEvent.setCancelled(true);
                        return;
                    }

                    Task task = refreshTask.get();
                    if (task != null && task.isAlive()) return;
                    refreshTask.set(
                            MinecraftServer.getSchedulerManager()
                                    .buildTask(() -> {
                                        Map<Integer, Button> slotMap = buttonMap.getButtons();
                                        currentSlotMap.set(slotMap);
                                        for (int i = 0; i < getSize(); i++) {
                                            Button button = slotMap.get(i);
                                            if (button == null) {
                                                setItemStack(i, ItemStack.AIR);
                                            } else {
                                                setItemStack(i, button.getItem());
                                            }
                                        }
                                    })
                                    .repeat(TaskUtil.tick(updateTick))
                                    .schedule()
                    );
                })
                .addListener(InventoryCloseEvent.class, closeEvent -> {
                    if (!Objects.equals(closeEvent.getInventory(), this)) return;
                    if (!closeHandler.test(closeEvent.getPlayer())) {
                        closeEvent.setNewInventory(this);
                        return;
                    }

                    if (getViewers().size() > 1) return;
                    Optional.ofNullable(refreshTask.get()).ifPresent(Task::cancel);
                });
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private InventoryType inventoryType;
        private Component title;
        private ButtonMap buttonMap;
        private int updateTick;
        private boolean cancelClickByDefault;
        private OpenHandler openHandler;
        private CloseHandler closeHandler;

        private Builder() {
            updateTick = 1;
            cancelClickByDefault = true;
            openHandler = player -> true;
            closeHandler = player -> true;
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

        public Builder setUpdateTick(int updateTick) {
            this.updateTick = updateTick;
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
                    updateTick,
                    cancelClickByDefault,
                    openHandler,
                    closeHandler
            );
        }
    }
}
