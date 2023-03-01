package me.hsgamer.flexegames.helper.property.editor;

import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.hscore.minecraft.gui.button.Button;
import me.hsgamer.hscore.minecraft.gui.button.impl.DummyButton;
import me.hsgamer.hscore.minecraft.gui.button.impl.SimpleButton;
import me.hsgamer.hscore.minecraft.gui.mask.impl.ButtonMapMask;
import me.hsgamer.hscore.minecraft.gui.mask.impl.StaticButtonPaginatedMask;

public abstract class SinglePropertyEditor extends SimplePropertyEditor {
    @Override
    protected ButtonMapMask getActionMask(StaticButtonPaginatedMask valueMask) {
        Button completeButton = new SimpleButton(ItemUtil.asMinestomItem(getCompleteItem()), event -> complete(event.getViewerID()));
        Button dummyButton = new DummyButton(ItemUtil.asMinestomItem(getDummyItem()));
        return new ButtonMapMask("action")
                .addButton(dummyButton, 18, 19, 20, 21, 22, 23, 24, 25)
                .addButton(completeButton, 26);
    }
}
