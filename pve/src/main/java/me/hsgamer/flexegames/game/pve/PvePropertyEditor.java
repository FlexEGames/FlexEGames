package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.helper.property.button.SinglePropertyButton;
import me.hsgamer.flexegames.helper.property.PropertyButton;
import me.hsgamer.flexegames.helper.property.editor.SinglePropertyEditor;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class PvePropertyEditor extends SinglePropertyEditor {
    private final PveExtension pveExtension;

    public PvePropertyEditor(PveExtension pveExtension) {
        this.pveExtension = pveExtension;
        setTitle(pveExtension.getMessageConfig().getEditorTitle());
    }

    @Override
    protected ItemStack getCompleteItem() {
        return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorComplete()));
    }

    @Override
    protected List<PropertyButton<?>> getPropertyButtons() {
        var kitEdit = new SinglePropertyButton<>(this, PveProperties.KIT) {
            @Override
            protected String newValue(UUID uuid, String currentValue) {
                List<String> list = new ArrayList<>(pveExtension.getGameKitManager().getGameKitMap().keySet());
                if (list.isEmpty()) {
                    return currentValue;
                }
                int index = list.indexOf(currentValue);
                if (index == -1) {
                    return list.get(0);
                }
                if (index == list.size() - 1) {
                    return list.get(0);
                }
                return list.get(index + 1);
            }

            @Override
            protected ItemStack display(String value) {
                return pveExtension.getGameKitManager().getGameKit(value).getDisplayItem();
            }
        };
        var pvpEdit = new SinglePropertyButton<>(this, PveProperties.LEGACY_PVP) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorLegacyPvp(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }

            @Override
            protected Boolean newValue(UUID uuid, Boolean currentValue) {
                return !currentValue;
            }
        };
        var toughMobEdit = new SinglePropertyButton<>(this, PveProperties.TOUGH_MOB) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorToughMob(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }

            @Override
            protected Boolean newValue(UUID uuid, Boolean currentValue) {
                return !currentValue;
            }
        };
        var healOnRestEdit = new SinglePropertyButton<>(this, PveProperties.HEAL_ON_REST) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorHealOnRest(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }

            @Override
            protected Boolean newValue(UUID uuid, Boolean currentValue) {
                return !currentValue;
            }
        };
        return List.of(kitEdit, pvpEdit, toughMobEdit, healOnRestEdit);
    }
}
