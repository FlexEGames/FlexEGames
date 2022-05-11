package me.hsgamer.flexegames.hook;

import io.github.bloepiloepi.pvp.enchantment.CustomEnchantments;
import io.github.bloepiloepi.pvp.entities.Tracker;
import io.github.bloepiloepi.pvp.potion.effect.CustomPotionEffects;
import io.github.bloepiloepi.pvp.potion.item.CustomPotionTypes;
import lombok.experimental.UtilityClass;
import net.minestom.server.event.Event;
import net.minestom.server.event.EventNode;
import net.minestom.server.item.Material;
import net.minestom.server.registry.Registry;

import java.lang.reflect.Field;

@UtilityClass
public final class PvpHook {
    public static void hook(EventNode<Event> node) {
        CustomEnchantments.registerAll();
        CustomPotionEffects.registerAll();
        CustomPotionTypes.registerAll();

        Tracker.register(node);

        try {
            Field isFood = Registry.MaterialEntry.class.getDeclaredField("isFood");
            isFood.setAccessible(true);
            isFood.set(Material.POTION.registry(), true);
            isFood.set(Material.MILK_BUCKET.registry(), true);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            e.printStackTrace();
        }
    }
}
