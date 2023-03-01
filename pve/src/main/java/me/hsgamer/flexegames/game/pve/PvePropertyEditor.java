package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.helper.property.PropertyButton;
import me.hsgamer.flexegames.helper.property.button.BooleanPropertyButton;
import me.hsgamer.flexegames.helper.property.button.ListPropertyButton;
import me.hsgamer.flexegames.helper.property.editor.SinglePropertyEditor;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
        var kitEdit = new ListPropertyButton<>(this, PveProperties.KIT, new ArrayList<>(pveExtension.getGameKitManager().getGameKitMap().keySet())) {
            @Override
            protected ItemStack display(String value) {
                return pveExtension.getGameKitManager().getGameKit(value).getDisplayItem();
            }
        };
        var pvpEdit = new BooleanPropertyButton(this, PveProperties.LEGACY_PVP) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorLegacyPvp(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }
        };
        var toughMobEdit = new BooleanPropertyButton(this, PveProperties.TOUGH_MOB) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorToughMob(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }
        };
        var healOnRestEdit = new BooleanPropertyButton(this, PveProperties.HEAL_ON_REST) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(pveExtension.getMessageConfig().getEditorHealOnRest(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }
        };
        return List.of(kitEdit, pvpEdit, toughMobEdit, healOnRestEdit);
    }
}
