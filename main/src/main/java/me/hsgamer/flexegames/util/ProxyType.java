package me.hsgamer.flexegames.util;

import me.hsgamer.flexegames.GameServer;
import net.minestom.server.extras.MojangAuth;
import net.minestom.server.extras.bungee.BungeeCordProxy;
import net.minestom.server.extras.velocity.VelocityProxy;

import java.util.function.Consumer;

/**
 * The type of proxy
 */
public enum ProxyType {
    /**
     * The offline-mode proxy
     */
    NONE,
    /**
     * The BungeeCord proxy
     */
    BUNGEE(BungeeCordProxy::enable),
    /**
     * The Velocity proxy
     */
    VELOCITY(server -> {
        String secret = server.getMainConfig().getVelocitySecret();
        if (secret.isEmpty()) {
            throw new IllegalStateException("Velocity secret is empty");
        }
        VelocityProxy.enable(secret);
    }),
    /**
     * The Mojang proxy
     */
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

    /**
     * Enable the proxy
     *
     * @param gameServer the game server
     */
    public void execute(GameServer gameServer) {
        proxyExecutor.accept(gameServer);
    }
}
