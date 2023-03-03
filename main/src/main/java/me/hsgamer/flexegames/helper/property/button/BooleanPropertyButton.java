package me.hsgamer.flexegames.helper.property.button;

import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.helper.property.PropertyEditor;

import java.util.UUID;

/**
 * The button for boolean property
 */
public abstract class BooleanPropertyButton extends SinglePropertyButton<Boolean> {
    /**
     * Create a new button
     *
     * @param propertyEditor   the property editor
     * @param propertyKeyValue the property key-value
     */
    protected BooleanPropertyButton(PropertyEditor propertyEditor, PropertyKeyValue<Boolean> propertyKeyValue) {
        super(propertyEditor, propertyKeyValue);
    }

    @Override
    protected Boolean newValue(UUID uuid, Boolean currentValue) {
        return !currentValue;
    }
}
