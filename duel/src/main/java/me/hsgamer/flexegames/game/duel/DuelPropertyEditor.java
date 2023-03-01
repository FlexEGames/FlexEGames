package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.helper.editor.PropertySingleValueButton;
import me.hsgamer.flexegames.helper.editor.PropertyValueButton;
import me.hsgamer.flexegames.helper.editor.SimplePropertyEditor;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class DuelPropertyEditor extends SimplePropertyEditor {
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
    protected List<PropertyValueButton<?>> getValueButtons() {
        var worldEdit = new PropertySingleValueButton<>(this, DuelProperties.WORLD) {
            @Override
            protected String newValue(UUID uuid, String currentValue) {
                List<String> list = new ArrayList<>(duelExtension.getDuelWorldManager().getDuelWorldMap().keySet());
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
                return duelExtension.getDuelWorldManager().getDuelWorld(value).getDisplayItem();
            }
        };
        var kitEdit = new PropertySingleValueButton<>(this, DuelProperties.KIT) {
            @Override
            protected String newValue(UUID uuid, String currentValue) {
                List<String> list = new ArrayList<>(duelExtension.getGameKitManager().getGameKitMap().keySet());
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
                return duelExtension.getGameKitManager().getGameKit(value).getDisplayItem();
            }
        };
        var pvpEdit = new PropertySingleValueButton<>(this, DuelProperties.LEGACY_PVP) {
            @Override
            protected ItemStack display(Boolean value) {
                return ItemUtil.stripItalics(ItemBuilder.buildItem(duelExtension.getMessageConfig().getEditorLegacyPvp(), Map.of("value", () -> Component.text(Boolean.toString(value)))));
            }

            @Override
            protected Boolean newValue(UUID uuid, Boolean currentValue) {
                return !currentValue;
            }
        };

        return List.of(worldEdit, kitEdit, pvpEdit);
    }
}
