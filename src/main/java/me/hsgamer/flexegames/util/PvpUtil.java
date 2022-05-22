package me.hsgamer.flexegames.util;

import io.github.bloepiloepi.pvp.PvpExtension;
import io.github.bloepiloepi.pvp.explosion.PvpExplosionSupplier;
import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.player.GamePlayer;
import net.minestom.server.MinecraftServer;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.trait.EntityEvent;
import net.minestom.server.instance.Instance;

@UtilityClass
public final class PvpUtil {
    private static boolean initPvp = false;

    private static void initPvp() {
        if (!initPvp) {
            PvpExtension.init();
            MinecraftServer.getConnectionManager().setPlayerProvider(GamePlayer::new);
            initPvp = true;
        }
    }

    public static EventNode<EntityEvent> applyPvp(EventNode<EntityEvent> node, boolean legacy) {
        initPvp();
        return node.addChild(legacy ? PvpExtension.legacyEvents() : PvpExtension.events());
    }

    public static void applyExplosion(Instance instance) {
        initPvp();
        instance.setExplosionSupplier(PvpExplosionSupplier.INSTANCE);
    }
}
