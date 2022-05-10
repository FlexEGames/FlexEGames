package me.hsgamer.epicmegagames.hook;

import lombok.experimental.UtilityClass;
import me.hsgamer.epicmegagames.manager.ReplacementManager;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.event.server.ServerTickMonitorEvent;
import net.minestom.server.monitoring.TickMonitor;
import net.minestom.server.utils.MathUtils;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

@UtilityClass
public final class TickMonitorHook {
    private static final AtomicReference<TickMonitor> lastTick = new AtomicReference<>();

    public static void hook() {
        MinecraftServer.getGlobalEventHandler().addListener(ServerTickMonitorEvent.class, event -> lastTick.set(event.getTickMonitor()));

        ReplacementManager.addGlobalReplacement("ram_usage", () -> Component.text(Long.toString(getRamUsage())));
        ReplacementManager.addGlobalReplacement("tick_time", () -> TickMonitorHook.getLastTick().map(tickMonitor -> {
            double tick = MathUtils.round(tickMonitor.getTickTime(), 2);
            return Component.text(Double.toString(tick));
        }).orElse(Component.text("N/A")));
    }

    public static Optional<TickMonitor> getLastTick() {
        return Optional.ofNullable(lastTick.get());
    }

    public static long getRamUsage() {
        Runtime runtime = Runtime.getRuntime();
        return (runtime.totalMemory() - runtime.freeMemory()) / 1024 / 1024;
    }
}
