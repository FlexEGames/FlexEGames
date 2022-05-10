package me.hsgamer.flexgames.util;

import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.minestom.server.item.ItemStack;

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
}
