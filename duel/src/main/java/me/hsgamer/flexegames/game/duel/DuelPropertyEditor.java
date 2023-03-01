package me.hsgamer.flexegames.game.duel;

import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.helper.property.PropertyButton;
import me.hsgamer.flexegames.helper.property.button.SinglePropertyButton;
import me.hsgamer.flexegames.helper.property.editor.SinglePropertyEditor;
import me.hsgamer.flexegames.util.ItemUtil;
import net.kyori.adventure.text.Component;
import net.minestom.server.item.ItemStack;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

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
        var worldEdit = new SinglePropertyButton<>(this, DuelProperties.WORLD) {
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

            @Override
            protected void onOpen(UUID uuid) {
                if (isSet(uuid)) return;
                List<String> worldNames = new ArrayList<>(duelExtension.getDuelWorldManager().getDuelWorldMap().keySet());
                if (!worldNames.isEmpty()) {
                    setValue(uuid, worldNames.get(0));
                }
            }
        };
        var kitEdit = new SinglePropertyButton<>(this, DuelProperties.KIT) {
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

            @Override
            protected void onOpen(UUID uuid) {
                if (isSet(uuid)) return;
                List<String> kitNames = new ArrayList<>(duelExtension.getGameKitManager().getGameKitMap().keySet());
                if (!kitNames.isEmpty()) {
                    setValue(uuid, kitNames.get(0));
                }
            }
        };
        var pvpEdit = new SinglePropertyButton<>(this, DuelProperties.LEGACY_PVP) {
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
