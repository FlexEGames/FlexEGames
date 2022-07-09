package me.hsgamer.flexegames.util;

import me.hsgamer.flexegames.config.MainConfig;
import net.minestom.server.extras.MojangAuth;
import net.minestom.server.extras.bungee.BungeeCordProxy;
import net.minestom.server.extras.velocity.VelocityProxy;

public enum ProxyType {
    NONE,
    BUNGEE(BungeeCordProxy::enable),
    VELOCITY(() -> {
        String secret = MainConfig.SERVER_VELOCITY_SECRET.getValue();
        if (secret.isEmpty()) {
            throw new IllegalStateException("Velocity secret is empty");
        }
        VelocityProxy.enable(secret);
    }),
    ONLINE(MojangAuth::init);
    private final Runnable proxyExecutor;

    ProxyType(Runnable proxyExecutor) {
        this.proxyExecutor = proxyExecutor;
    }

    ProxyType() {
        this(() -> {
        });
    }

    public void execute() {
        proxyExecutor.run();
    }
}
