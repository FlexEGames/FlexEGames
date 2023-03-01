package me.hsgamer.flexegames.helper.property;

import me.hsgamer.flexegames.api.property.PropertyKeyValue;
import me.hsgamer.flexegames.api.property.PropertyMap;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.hscore.minecraft.gui.button.Button;
import me.hsgamer.hscore.minecraft.gui.object.Item;
import me.hsgamer.hscore.ui.Display;
import net.minestom.server.item.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;
import java.util.UUID;

public abstract class PropertyButton<T> implements Button {
    protected final PropertyKeyValue<T> propertyKeyValue;
    private final PropertyEditor propertyEditor;

    protected PropertyButton(PropertyEditor propertyEditor, PropertyKeyValue<T> propertyKeyValue) {
        this.propertyEditor = propertyEditor;
        this.propertyKeyValue = propertyKeyValue;
        propertyEditor.addOnOpenListener(this::onOpen);
    }

    protected PropertyMap getPropertyMap(UUID uuid) {
        return propertyEditor.getPropertyMap(uuid);
    }

    protected abstract ItemStack display(T value);

    protected void onOpen(UUID uuid) {
        // NOOP
    }

    protected void setValue(UUID uuid, T value) {
        getPropertyMap(uuid).setProperty(propertyKeyValue, value);
        propertyEditor.getDisplay(uuid).ifPresent(Display::update);
    }

    protected T getValue(UUID uuid) {
        return Optional.ofNullable(getPropertyMap(uuid))
                .map(propertyMap -> propertyMap.getProperty(propertyKeyValue))
                .orElse(null);
    }

    protected boolean isSet(UUID uuid) {
        return Optional.ofNullable(getPropertyMap(uuid))
                .map(propertyKeyValue::has)
                .orElse(false);
    }

    @Override
    public final @Nullable Item getItem(@NotNull UUID uuid) {
        return Optional.ofNullable(getValue(uuid))
                .map(this::display)
                .map(ItemUtil::asMinestomItem)
                .orElse(null);
    }
}
