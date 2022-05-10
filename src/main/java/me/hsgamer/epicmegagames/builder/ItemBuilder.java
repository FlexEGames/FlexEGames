package me.hsgamer.epicmegagames.builder;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.common.CollectionUtils;
import me.hsgamer.hscore.common.Validate;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.minestom.server.item.Enchantment;
import net.minestom.server.item.ItemHideFlag;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Supplier;

@ExtensionMethod({Objects.class, ReplacementManager.class, CollectionUtils.class})
public class ItemBuilder extends Builder<Object, BiFunction<ItemStack, Map<String, Supplier<ComponentLike>>, ItemStack>> {
    public static final ItemBuilder INSTANCE = new ItemBuilder();

    private ItemBuilder() {
        register(o -> (itemStack, map) ->
                        Optional.ofNullable(Material.fromNamespaceId(o.toString("")))
                                .map(itemStack::withMaterial)
                                .orElse(itemStack),
                "material"
        );
        register(o -> (itemStack, map) -> {
            Component name = LegacyComponentSerializer.legacyAmpersand().deserialize(o.toString(""));
            name = name.replace(map);
            return itemStack.withDisplayName(name);
        }, "name");
        register(o -> (itemStack, map) -> {
            List<Component> lore = o.createStringListFromObject(false).stream()
                    .map(s -> LegacyComponentSerializer.legacyAmpersand().deserialize(s))
                    .map(component -> component.replace(map))
                    .toList();
            return itemStack.withLore(lore);
        }, "lore");
        register(o -> (itemStack, map) -> {
            var amount = 1;
            try {
                amount = Integer.parseInt(o.toString(""));
            } catch (NumberFormatException ignored) {
                // IGNORED
            }
            return itemStack.withAmount(amount);
        }, "amount");
        register(o -> (itemStack, map) -> {
            Map<Enchantment, Short> enchantments = new HashMap<>();
            var list = o.createStringListFromObject(true);
            for (var entry : list) {
                var split = entry.split(",", 2);
                var enchantmentName = split[0];
                short enchantmentLevel = 1;
                if (split.length > 1) {
                    try {
                        enchantmentLevel = Short.parseShort(split[1]);
                    } catch (NumberFormatException ignored) {
                        // IGNORED
                    }
                }
                Enchantment enchantment = Enchantment.fromNamespaceId(enchantmentName);
                if (enchantment != null) {
                    enchantments.put(enchantment, enchantmentLevel);
                }
            }
            return itemStack.withMeta(builder -> builder.enchantments(enchantments));
        }, "enchantments", "enchants", "enchant", "enchantment");
        register(o -> (itemStack, map) -> {
            List<ItemHideFlag> hideFlags = new ArrayList<>();
            var list = o.createStringListFromObject(true);
            for (var entry : list) {
                try {
                    ItemHideFlag hideFlag = ItemHideFlag.valueOf(entry.toUpperCase());
                    hideFlags.add(hideFlag);
                } catch (IllegalArgumentException ignored) {
                    // IGNORED
                }
            }
            return itemStack.withMeta(builder -> builder.hideFlag(hideFlags.toArray(new ItemHideFlag[0])));
        }, "item-flags", "flags", "flag", "itemflag", "item-flag", "itemflags");
        register(o -> (itemStack, map) -> {
            boolean unbreakable = Boolean.parseBoolean(o.toString(""));
            return itemStack.withMeta(builder -> builder.unbreakable(unbreakable));
        }, "unbreakable");
        register(o -> (itemStack, map) -> {
            var modelData = Validate.getNumber(o.toString("")).map(BigDecimal::intValue).orElse(0);
            return itemStack.withMeta(builder -> builder.customModelData(modelData));
        }, "custom-model-data", "model-data", "model");
        register(o -> (itemStack, map) -> {
            var damage = Validate.getNumber(o.toString("")).map(BigDecimal::intValue).orElse(0);
            return itemStack.withMeta(builder -> builder.damage(damage));
        }, "damage");
    }

    public static ItemStack buildItem(Map<String, Object> values, Map<String, Supplier<ComponentLike>> replacements) {
        ItemStack itemStack = ItemStack.of(Material.STONE);
        for (Map.Entry<String, Object> entry : values.entrySet()) {
            Optional<BiFunction<ItemStack, Map<String, Supplier<ComponentLike>>, ItemStack>> optional = INSTANCE.build(entry.getKey(), entry.getValue());
            if (optional.isPresent()) {
                itemStack = optional.get().apply(itemStack, replacements);
            }
        }
        return itemStack;
    }

    public static ItemStack buildItem(Map<String, Object> values) {
        return buildItem(values, Collections.emptyMap());
    }
}
