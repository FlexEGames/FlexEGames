package me.hsgamer.flexegames.helper.property.button;

import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.helper.property.PropertyEditor;

import java.util.List;
import java.util.UUID;

public abstract class ListPropertyButton<T> extends SinglePropertyButton<T> {
    private final List<T> valueList;

    protected ListPropertyButton(PropertyEditor propertyEditor, PropertyKeyValue<T> propertyKeyValue, List<T> valueList) {
        super(propertyEditor, propertyKeyValue);
        this.valueList = valueList;
    }

    @Override
    protected T newValue(UUID uuid, T currentValue) {
        if (valueList.isEmpty()) {
            return currentValue;
        }
        int index = valueList.indexOf(currentValue);
        if (index == -1) {
            return valueList.get(0);
        }
        if (index == valueList.size() - 1) {
            return valueList.get(0);
        }
        return valueList.get(index + 1);
    }

    @Override
    protected void onOpen(UUID uuid) {
        if (isSet(uuid)) return;
        if (!valueList.isEmpty()) {
            setValue(uuid, valueList.get(0));
        }
    }
}
