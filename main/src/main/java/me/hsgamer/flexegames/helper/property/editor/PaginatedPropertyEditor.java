package me.hsgamer.flexegames.helper.property.editor;

import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.hscore.minecraft.gui.GUIDisplay;
import me.hsgamer.hscore.minecraft.gui.button.Button;
import me.hsgamer.hscore.minecraft.gui.button.impl.DummyButton;
import me.hsgamer.hscore.minecraft.gui.button.impl.SimpleButton;
import me.hsgamer.hscore.minecraft.gui.mask.impl.ButtonMapMask;
import me.hsgamer.hscore.minecraft.gui.mask.impl.StaticButtonPaginatedMask;
import net.minestom.server.item.ItemStack;

/**
 * The {@link me.hsgamer.flexegames.helper.property.PropertyEditor} with pagination
 */
public abstract class PaginatedPropertyEditor extends SimplePropertyEditor {
    @Override
    protected ButtonMapMask getActionMask(StaticButtonPaginatedMask valueMask) {
        Button nextPageButton = new SimpleButton(ItemUtil.asMinestomItem(getNextPageItem()), event -> {
            valueMask.nextPage(event.getViewerID());
            getDisplay(event.getViewerID()).ifPresent(GUIDisplay::update);
        });
        Button previousPageButton = new SimpleButton(ItemUtil.asMinestomItem(getPreviousPageItem()), event -> {
            valueMask.previousPage(event.getViewerID());
            getDisplay(event.getViewerID()).ifPresent(GUIDisplay::update);
        });
        Button completeButton = new SimpleButton(ItemUtil.asMinestomItem(getCompleteItem()), event -> complete(event.getViewerID()));
        Button dummyButton = new DummyButton(ItemUtil.asMinestomItem(getDummyItem()));
        return new ButtonMapMask("action")
                .addButton(previousPageButton, 18)
                .addButton(nextPageButton, 19)
                .addButton(dummyButton, 20, 21, 22, 23, 24, 25)
                .addButton(completeButton, 26);
    }

    /**
     * Get the display item for the next page
     *
     * @return the display item
     */
    protected abstract ItemStack getNextPageItem();

    /**
     * Get the display item for the previous page
     *
     * @return the display item
     */
    protected abstract ItemStack getPreviousPageItem();
}
