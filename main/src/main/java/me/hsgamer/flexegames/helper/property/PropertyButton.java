package me.hsgamer.flexegames.helper.property;

import me.hsgamer.flexegames.api.property.GamePropertyKeyValue;
import me.hsgamer.flexegames.api.property.GamePropertyMap;
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
    protected final GamePropertyKeyValue<T> propertyKeyValue;
    private final PropertyEditor propertyEditor;

    protected PropertyButton(PropertyEditor propertyEditor, GamePropertyKeyValue<T> propertyKeyValue) {
        this.propertyEditor = propertyEditor;
        this.propertyKeyValue = propertyKeyValue;
        propertyEditor.addOnOpenListener(this::onOpen);
    }

    protected GamePropertyMap getGamePropertyMap(UUID uuid) {
        return propertyEditor.getPropertyMap(uuid);
    }

    protected abstract ItemStack display(T value);

    protected void onOpen(UUID uuid) {
        // NOOP
    }

    protected void setValue(UUID uuid, T value) {
        getGamePropertyMap(uuid).setProperty(propertyKeyValue, value);
        propertyEditor.getDisplay(uuid).ifPresent(Display::update);
    }

    protected T getValue(UUID uuid) {
        return Optional.ofNullable(getGamePropertyMap(uuid))
                .map(propertyMap -> propertyMap.getProperty(propertyKeyValue))
                .orElse(null);
    }

    protected boolean isSet(UUID uuid) {
        return Optional.ofNullable(getGamePropertyMap(uuid))
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
