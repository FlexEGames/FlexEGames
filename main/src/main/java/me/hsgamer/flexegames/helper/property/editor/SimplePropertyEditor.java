package me.hsgamer.flexegames.helper.property.editor;

import me.hsgamer.flexegames.helper.property.PropertyButton;
import me.hsgamer.flexegames.helper.property.PropertyEditor;
import me.hsgamer.hscore.minecraft.gui.advanced.AdvancedButtonMap;
import me.hsgamer.hscore.minecraft.gui.mask.MaskUtils;
import me.hsgamer.hscore.minecraft.gui.mask.impl.ButtonMapMask;
import me.hsgamer.hscore.minecraft.gui.mask.impl.StaticButtonPaginatedMask;
import net.kyori.adventure.text.Component;
import net.minestom.server.inventory.InventoryType;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;

import java.util.List;

/**
 * The simple {@link PropertyEditor}
 */
public abstract class SimplePropertyEditor extends PropertyEditor {
    @Override
    public void init() {
        setInventoryType(InventoryType.CHEST_3_ROW);

        AdvancedButtonMap buttonMap = new AdvancedButtonMap();
        setButtonMap(buttonMap);

        StaticButtonPaginatedMask valueMask = new StaticButtonPaginatedMask("value", MaskUtils.generateAreaSlots(0, 0, 8, 1).boxed().toList()).addButton(getPropertyButtons());
        ButtonMapMask actionMask = getActionMask(valueMask);
        buttonMap.addMask(valueMask);
        buttonMap.addMask(actionMask);

        super.init();
    }

    /**
     * Get the action mask
     *
     * @param valueMask the value mask
     * @return the action mask
     */
    protected abstract ButtonMapMask getActionMask(StaticButtonPaginatedMask valueMask);

    /**
     * Get the dummy item
     *
     * @return the dummy item
     */
    protected ItemStack getDummyItem() {
        return ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty());
    }

    /**
     * Get the complete item
     *
     * @return the complete item
     */
    protected abstract ItemStack getCompleteItem();

    /**
     * Get the property buttons
     *
     * @return the property buttons
     */
    protected abstract List<PropertyButton<?>> getPropertyButtons();
}
