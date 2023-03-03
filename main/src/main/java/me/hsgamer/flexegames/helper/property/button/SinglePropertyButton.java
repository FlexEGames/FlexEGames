package me.hsgamer.flexegames.helper.property.button;

import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.helper.property.PropertyButton;
import me.hsgamer.flexegames.helper.property.PropertyEditor;
import me.hsgamer.hscore.minecraft.gui.event.ClickEvent;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;
import java.util.UUID;

/**
 * The button for {@link PropertyEditor} with a single property
 *
 * @param <T> the type of the property
 */
public abstract class SinglePropertyButton<T> extends PropertyButton<T> {
    /**
     * Create a new button
     *
     * @param propertyEditor   the property editor
     * @param propertyKeyValue the property key-value
     */
    protected SinglePropertyButton(PropertyEditor propertyEditor, PropertyKeyValue<T> propertyKeyValue) {
        super(propertyEditor, propertyKeyValue);
    }

    /**
     * Get the new value when the button is clicked
     *
     * @param uuid         the uuid of the player
     * @param currentValue the current value
     * @return the new value
     */
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
