package me.hsgamer.flexegames.helper.property;

import me.hsgamer.flexegames.api.property.PropertyMap;
import me.hsgamer.hscore.minecraft.gui.event.OpenEvent;
import me.hsgamer.hscore.minestom.gui.MinestomGUIDisplay;
import me.hsgamer.hscore.minestom.gui.MinestomGUIHolder;
import net.minestom.server.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

public abstract class PropertyEditor extends MinestomGUIHolder {
    private final Map<UUID, PropertyMapFuture> propertyMapFutureMap = new ConcurrentHashMap<>();
    private final List<Consumer<UUID>> onOpenListeners = new ArrayList<>();

    public void addOnOpenListener(Consumer<UUID> listener) {
        onOpenListeners.add(listener);
    }

    protected PropertyMap getPropertyMap(UUID uuid) {
        return Optional.ofNullable(propertyMapFutureMap.get(uuid))
                .map(PropertyMapFuture::current)
                .orElse(null);
    }

    protected void complete(UUID uuid) {
        Optional.ofNullable(propertyMapFutureMap.get(uuid))
                .ifPresent(PropertyMapFuture::complete);
    }

    @Override
    protected void onRemoveDisplay(@NotNull MinestomGUIDisplay display) {
        Optional.ofNullable(propertyMapFutureMap.remove(display.getUniqueId()))
                .filter(future -> !future.future().isDone())
                .ifPresent(PropertyMapFuture::cancel);
    }

    @Override
    protected void onOpen(@NotNull OpenEvent event) {
        onOpenListeners.forEach(listener -> listener.accept(event.getViewerID()));
    }

    @Override
    public void stop() {
        super.stop();
        propertyMapFutureMap.values().forEach(PropertyMapFuture::cancel);
        propertyMapFutureMap.clear();
    }

    public CompletableFuture<PropertyMap> open(Player player, PropertyMap propertyMap) {
        PropertyMap clonedPropertyMap = propertyMap.clone();
        CompletableFuture<PropertyMap> future = new CompletableFuture<>();
        var oldFuture = propertyMapFutureMap.put(player.getUuid(), new PropertyMapFuture(clonedPropertyMap, future));
        if (oldFuture != null) {
            oldFuture.cancel();
        }
        createDisplay(player.getUuid()).open();
        return future;
    }

    private record PropertyMapFuture(PropertyMap current, CompletableFuture<PropertyMap> future) {
        private void complete() {
            future.complete(current);
        }

        private void cancel() {
            future.cancel(true);
        }
    }
}
