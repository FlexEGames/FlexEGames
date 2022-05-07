package me.hsgamer.epicmegagames.builder;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import me.hsgamer.hscore.builder.Builder;
import me.hsgamer.hscore.common.CollectionUtils;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;

@ExtensionMethod({Objects.class, ReplacementManager.class, CollectionUtils.class})
public class ItemModifierBuilder extends Builder<Object, BiFunction<ItemStack, Map<String, ComponentLike>, ItemStack>> {
    public static final ItemModifierBuilder INSTANCE = new ItemModifierBuilder();

    private ItemModifierBuilder() {
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
                },
                "name"
        );
        register(o -> (itemStack, map) -> {
            List<Component> lore = o.createStringListFromObject(false).stream()
                    .map(s -> LegacyComponentSerializer.legacyAmpersand().deserialize(s))
                    .map(component -> component.replace(map))
                    .toList();
            return itemStack.withLore(lore);
        }, "lore");
        register(o -> (itemStack, map) -> {
            int amount = 1;
            try {
                amount = Integer.parseInt(o.toString(""));
            } catch (NumberFormatException ignored) {
                // IGNORED
            }
            return itemStack.withAmount(amount);
        }, "amount");
    }

    public static ItemStack buildItem(Map<String, Object> values, Map<String, ComponentLike> replacements) {
        ItemStack itemStack = ItemStack.of(Material.STONE);
        for (Map.Entry<String, Object> entry : values.entrySet()) {
            Optional<BiFunction<ItemStack, Map<String, ComponentLike>, ItemStack>> optional = INSTANCE.build(entry.getKey(), entry.getValue());
            if (optional.isPresent()) {
                itemStack = optional.get().apply(itemStack, replacements);
            }
        }
        return itemStack;
    }
}
