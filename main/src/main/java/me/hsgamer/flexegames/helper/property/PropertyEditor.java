package me.hsgamer.flexegames.helper.property;

import me.hsgamer.flexegames.api.property.GamePropertyMap;
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
    private final Map<UUID, GamePropertyMapFuture> propertyMapFutureMap = new ConcurrentHashMap<>();
    private final List<Consumer<UUID>> onOpenListeners = new ArrayList<>();

    public void addOnOpenListener(Consumer<UUID> listener) {
        onOpenListeners.add(listener);
    }

    protected GamePropertyMap getPropertyMap(UUID uuid) {
        return Optional.ofNullable(propertyMapFutureMap.get(uuid))
                .map(GamePropertyMapFuture::current)
                .orElse(null);
    }

    protected void complete(UUID uuid) {
        Optional.ofNullable(propertyMapFutureMap.get(uuid))
                .ifPresent(GamePropertyMapFuture::complete);
    }

    @Override
    protected void onRemoveDisplay(@NotNull MinestomGUIDisplay display) {
        Optional.ofNullable(propertyMapFutureMap.remove(display.getUniqueId()))
                .filter(future -> !future.future().isDone())
                .ifPresent(GamePropertyMapFuture::cancel);
    }

    @Override
    protected void onOpen(@NotNull OpenEvent event) {
        onOpenListeners.forEach(listener -> listener.accept(event.getViewerID()));
    }

    @Override
    public void stop() {
        super.stop();
        propertyMapFutureMap.values().forEach(GamePropertyMapFuture::cancel);
        propertyMapFutureMap.clear();
    }

    public CompletableFuture<GamePropertyMap> open(Player player, GamePropertyMap propertyMap) {
        GamePropertyMap clonedPropertyMap = propertyMap.clone();
        CompletableFuture<GamePropertyMap> future = new CompletableFuture<>();
        var oldFuture = propertyMapFutureMap.put(player.getUuid(), new GamePropertyMapFuture(clonedPropertyMap, future));
        if (oldFuture != null) {
            oldFuture.cancel();
        }
        createDisplay(player.getUuid()).open();
        return future;
    }

    private record GamePropertyMapFuture(GamePropertyMap current, CompletableFuture<GamePropertyMap> future) {
        private void complete() {
            future.complete(current);
        }

        private void cancel() {
            future.cancel(true);
        }
    }
}
