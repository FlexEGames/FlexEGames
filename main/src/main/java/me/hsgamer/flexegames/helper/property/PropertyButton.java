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

/**
 * The button to edit a property in {@link PropertyMap}
 *
 * @param <T> the type of the property
 */
public abstract class PropertyButton<T> implements Button {
    /**
     * The property key-value
     */
    protected final PropertyKeyValue<T> propertyKeyValue;
    private final PropertyEditor propertyEditor;

    /**
     * Create a new button
     *
     * @param propertyEditor   the property editor
     * @param propertyKeyValue the property key-value
     */
    protected PropertyButton(PropertyEditor propertyEditor, PropertyKeyValue<T> propertyKeyValue) {
        this.propertyEditor = propertyEditor;
        this.propertyKeyValue = propertyKeyValue;
        propertyEditor.addOnOpenListener(this::onOpen);
    }

    /**
     * Get the property map
     *
     * @param uuid the uuid
     * @return the property map
     */
    protected PropertyMap getPropertyMap(UUID uuid) {
        return propertyEditor.getPropertyMap(uuid);
    }

    /**
     * Display the value
     *
     * @param value the value
     * @return the item stack
     */
    protected abstract ItemStack display(T value);

    /**
     * Called when the property editor is opened
     *
     * @param uuid the uuid
     */
    protected void onOpen(UUID uuid) {
        // EMPTY
    }

    /**
     * Set the value
     *
     * @param uuid  the uuid
     * @param value the value
     */
    protected void setValue(UUID uuid, T value) {
        getPropertyMap(uuid).setProperty(propertyKeyValue, value);
        propertyEditor.getDisplay(uuid).ifPresent(Display::update);
    }

    /**
     * Get the value
     *
     * @param uuid the uuid
     * @return the value
     */
    protected T getValue(UUID uuid) {
        return Optional.ofNullable(getPropertyMap(uuid))
                .map(propertyMap -> propertyMap.getProperty(propertyKeyValue))
                .orElse(null);
    }

    /**
     * Check if the property is set
     *
     * @param uuid the uuid
     * @return true if the property is set
     */
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
