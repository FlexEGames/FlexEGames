package me.hsgamer.flexegames.util;

import me.hsgamer.flexegames.GameServer;
import net.minestom.server.extras.MojangAuth;
import net.minestom.server.extras.bungee.BungeeCordProxy;
import net.minestom.server.extras.velocity.VelocityProxy;

import java.util.function.Consumer;

public enum ProxyType {
    NONE,
    BUNGEE(BungeeCordProxy::enable),
    VELOCITY(server -> {
        String secret = server.getMainConfig().getVelocitySecret();
        if (secret.isEmpty()) {
            throw new IllegalStateException("Velocity secret is empty");
        }
        VelocityProxy.enable(secret);
    }),
    ONLINE(MojangAuth::init);
    private final Consumer<GameServer> proxyExecutor;

    ProxyType(Consumer<GameServer> proxyExecutor) {
        this.proxyExecutor = proxyExecutor;
    }

    ProxyType(Runnable proxyExecutor) {
        this(server -> proxyExecutor.run());
    }

    ProxyType() {
        this(gameServer -> {
        });
    }

    public void execute(GameServer gameServer) {
        proxyExecutor.accept(gameServer);
    }
}
