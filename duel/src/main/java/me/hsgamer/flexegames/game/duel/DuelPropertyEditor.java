package me.hsgamer.flexegames.game.duel;

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

public class DuelPropertyEditor extends SinglePropertyEditor {
    private final DuelExtension duelExtension;

    public DuelPropertyEditor(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
        setTitle(duelExtension.getMessageConfig().getEditorTitle());
    }

    @Override
    protected ItemStack getCompleteItem() {
        return ItemUtil.stripItalics(ItemBuilder.buildItem(duelExtension.getMessageConfig().getEditorComplete()));
    }

    @Override
    protected List<PropertyButton<?>> getPropertyButtons() {
        var worldEdit = new ListPropertyButton<>(this, DuelProperties.WORLD, new ArrayList<>(duelExtension.getDuelWorldManager().getDuelWorldMap().keySet())) {
            @Override
            protected ItemStack display(String value) {
                return duelExtension.getDuelWorldManager().getDuelWorld(value).getDisplayItem();
            }
        };
        var kitEdit = new ListPropertyButton<>(this, DuelProperties.KIT, new ArrayList<>(duelExtension.getGameKitManager().getGameKitMap().keySet())) {
            @Override
            protected ItemStack display(String value) {
                return duelExtension.getGameKitManager().getGameKit(value).getDisplayItem();
            }
        };
        var pvpEdit = new BooleanPropertyButton(this, DuelProperties.LEGACY_PVP) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(duelExtension.getMessageConfig().getEditorLegacyPvp(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }
        };

        return List.of(worldEdit, kitEdit, pvpEdit);
    }
}
