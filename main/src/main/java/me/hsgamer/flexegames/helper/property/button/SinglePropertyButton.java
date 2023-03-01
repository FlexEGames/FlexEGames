package me.hsgamer.flexegames.helper.property.button;

import me.hsgamer.flexegames.api.property.GamePropertyKeyValue;
import me.hsgamer.flexegames.helper.property.PropertyButton;
import me.hsgamer.flexegames.helper.property.PropertyEditor;
import me.hsgamer.hscore.minecraft.gui.event.ClickEvent;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;
import java.util.UUID;

public abstract class SinglePropertyButton<T> extends PropertyButton<T> {
    protected SinglePropertyButton(PropertyEditor propertyEditor, GamePropertyKeyValue<T> propertyKeyValue) {
        super(propertyEditor, propertyKeyValue);
    }

    protected abstract T newValue(UUID uuid, T currentValue);

    @Override
    public void handleAction(@NotNull ClickEvent event) {
        UUID uuid = event.getViewerID();
        T currentValue = getValue(uuid);
        T newValue = newValue(uuid, currentValue);
        if (!Objects.equals(currentValue, newValue)) {
            setValue(uuid, newValue);
        }
    }
}
