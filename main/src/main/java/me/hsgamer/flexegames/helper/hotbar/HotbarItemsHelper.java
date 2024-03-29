package me.hsgamer.flexegames.helper.hotbar;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.hscore.common.Validate;
import net.minestom.server.MinecraftServer;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.inventory.InventoryPreClickEvent;
import net.minestom.server.event.item.ItemDropEvent;
import net.minestom.server.event.player.*;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.event.trait.PlayerEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.inventory.TransactionOption;
import net.minestom.server.item.ItemStack;
import net.minestom.server.tag.Tag;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import static net.minestom.server.event.EventListener.builder;

/**
 * The helper class for hotbar items
 */
public class HotbarItemsHelper {
    private final Instance instance;
    private final EventNode<PlayerEvent> playerNode;
    private final EventNode<InstanceEvent> instanceNode;
    private final AtomicBoolean isEnabled = new AtomicBoolean(false);
    private final Map<Integer, ItemStack> itemSlots = new HashMap<>();
    private final Map<ItemStack, Consumer<Player>> itemConsumers = new HashMap<>();
    private final Tag<Boolean> tag = Tag.Boolean("flexegames:hotbar-items-helper").defaultValue(false);

    /**
     * Create a new helper
     *
     * @param instance the instance
     */
    public HotbarItemsHelper(Instance instance) {
        this.instance = instance;
        this.playerNode = EventNode.event("hotbar-" + UUID.randomUUID(), EventFilter.PLAYER, event -> event.getPlayer().getInstance() == instance && isEnabled.get());
        this.instanceNode = EventNode.event("hotbarInstance-" + UUID.randomUUID(), EventFilter.INSTANCE, event -> event.getInstance() == instance && isEnabled.get());

        playerNode
                .addListener(PlayerSpawnEvent.class, event -> giveItems(event.getPlayer()));
        instanceNode
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    if (event.getEntity() instanceof Player player) {
                        clearItems(player);
                    }
                })
                .addListener(builder(PlayerSwapItemEvent.class)
                        .handler(event -> event.setCancelled(true))
                        .filter(event -> event.getMainHandItem().getTag(tag) || event.getOffHandItem().getTag(tag))
                        .build())
                .addListener(builder(InventoryPreClickEvent.class)
                        .handler(event -> event.setCancelled(true))
                        .filter(event -> event.getClickedItem().getTag(tag) || event.getCursorItem().getTag(tag))
                        .build())
                .addListener(builder(PlayerBlockPlaceEvent.class)
                        .handler(event -> event.setCancelled(true))
                        .filter(event -> event.getPlayer().getItemInHand(event.getHand()).getTag(tag))
                        .build())
                .addListener(builder(PlayerBlockBreakEvent.class)
                        .handler(event -> event.setCancelled(true))
                        .filter(event -> event.getPlayer().getItemInMainHand().getTag(tag))
                        .build())
                .addListener(builder(ItemDropEvent.class)
                        .handler(event -> event.setCancelled(true))
                        .filter(event -> event.getItemStack().getTag(tag))
                        .build())
                .addListener(builder(PlayerUseItemEvent.class)
                        .handler(event -> {
                            event.setCancelled(true);
                            Optional.ofNullable(itemConsumers.get(event.getItemStack()))
                                    .ifPresent(consumer -> consumer.accept(event.getPlayer()));
                        })
                        .filter(event -> event.getPlayer().getInventory().getItemInHand(event.getHand()).getTag(tag))
                        .build())
                .addListener(builder(PlayerBlockInteractEvent.class)
                        .handler(event -> {
                            event.setCancelled(true);
                            event.setBlockingItemUse(true);
                            Optional.ofNullable(itemConsumers.get(event.getPlayer().getInventory().getItemInMainHand()))
                                    .ifPresent(consumer -> consumer.accept(event.getPlayer()));
                        })
                        .filter(event -> event.getPlayer().getInventory().getItemInHand(event.getHand()).getTag(tag))
                        .build())
                .addListener(builder(PlayerHandAnimationEvent.class)
                        .handler(event -> {
                            event.setCancelled(true);
                            Optional.ofNullable(itemConsumers.get(event.getPlayer().getInventory().getItemInMainHand()))
                                    .ifPresent(consumer -> consumer.accept(event.getPlayer()));
                        })
                        .filter(event -> event.getPlayer().getInventory().getItemInHand(event.getHand()).getTag(tag))
                        .build());
    }

    /**
     * Initialize the helper
     */
    public void init() {
        MinecraftServer.getGlobalEventHandler().addChild(playerNode);
        instance.eventNode().addChild(instanceNode);
    }

    /**
     * Clear the helper
     */
    public void clear() {
        itemSlots.clear();
        itemConsumers.clear();
        MinecraftServer.getGlobalEventHandler().removeChild(playerNode);
        instance.eventNode().removeChild(instanceNode);
    }

    /**
     * Check if the helper is enabled
     *
     * @return true if enabled
     */
    public boolean isEnabled() {
        return isEnabled.get();
    }

    /**
     * Enable/Disable the helper.
     * If the helper is enabled, the necessary events will be active.
     * If the helper is disabled, the necessary events will be disabled.
     *
     * @param enabled true to enable
     */
    public void setEnabled(boolean enabled) {
        isEnabled.set(enabled);
    }

    /**
     * Add an item to the hotbar
     *
     * @param slot      the slot
     * @param itemStack the item
     * @param consumer  the consumer when the item is interacted with
     */
    public void registerHotbarItem(int slot, ItemStack itemStack, Consumer<Player> consumer) {
        itemStack = itemStack.withTag(tag, true);
        itemSlots.put(slot, itemStack);
        itemConsumers.put(itemStack, consumer);
    }

    /**
     * Add an item to the hotbar from the setting map
     *
     * @param map         the map
     * @param defaultSlot the default slot
     * @param consumer    the consumer when the item is interacted with
     */
    public void registerHotbarItemFromMap(Map<String, Object> map, int defaultSlot, Consumer<Player> consumer) {
        var item = ItemUtil.stripItalics(ItemBuilder.buildItem(map));
        var slot = Optional.ofNullable(map.get("slot")).map(Objects::toString).flatMap(Validate::getNumber).map(BigDecimal::intValue).orElse(defaultSlot);
        boolean enable = Optional.ofNullable(map.get("enable")).map(Objects::toString).map(Boolean::parseBoolean).orElse(true);
        if (enable) {
            registerHotbarItem(slot, item, consumer);
        }
    }

    /**
     * Give the items to the player
     *
     * @param player the player
     */
    public void giveItems(Player player) {
        if (!isEnabled.get()) return;
        for (var entry : itemSlots.entrySet()) {
            player.getInventory().setItemStack(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Clear the items from the player
     *
     * @param player the player
     */
    public void clearItems(Player player) {
        var inventory = player.getInventory();
        for (ItemStack item : inventory.getItemStacks()) {
            if (Boolean.TRUE.equals(item.getTag(tag)))
                player.getInventory().takeItemStack(item, TransactionOption.ALL);
        }
    }
}
