package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.hscore.minestom.gui.object.MinestomItem;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.Map;

@UtilityClass
public final class ItemUtil {
    public static Component stripItalics(Component component) {
        if (component == null) return null;

        if (component.decoration(TextDecoration.ITALIC) == TextDecoration.State.NOT_SET) {
            component = component.decoration(TextDecoration.ITALIC, false);
        }

        return component;
    }

    public static ItemStack stripItalics(ItemStack itemStack) {
        if (itemStack == null) return null;

        return itemStack
                .withDisplayName(ItemUtil::stripItalics)
                .withLore(lore -> lore.stream().map(ItemUtil::stripItalics).toList());
    }

    public static MinestomItem asMinestomItem(ItemStack itemStack) {
        return new MinestomItem(itemStack);
    }

    public static ItemStack getItemOrStone(Object object) {
        if (object instanceof Map<?, ?> rawMap) {
            return ItemUtil.stripItalics(ItemBuilder.buildItem(MapUtil.toStringObjectMap(rawMap)));
        }
        String materialName = String.valueOf(object);
        Material material = Material.fromNamespaceId(materialName);
        if (material != null) {
            return ItemStack.of(material);
        }
        return ItemStack.of(Material.STONE);
    }
}
