package me.hsgamer.epicmegagames.lobby;

import me.hsgamer.epicmegagames.config.LobbyConfig;
import me.hsgamer.epicmegagames.util.FullBrightDimension;
import me.hsgamer.epicmegagames.util.LoaderType;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.item.ItemDropEvent;
import net.minestom.server.event.player.PlayerBlockBreakEvent;
import net.minestom.server.event.player.PlayerBlockPlaceEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.instance.InstanceContainer;
import org.jglrxavpok.hephaistos.nbt.NBTCompound;

import java.util.UUID;

public class Lobby extends InstanceContainer {
    public static final Lobby INSTANCE = new Lobby();

    static {
        MinecraftServer.getInstanceManager().registerInstance(INSTANCE);
    }

    private final Pos position;

    private Lobby() {
        super(UUID.randomUUID(), FullBrightDimension.INSTANCE);
        position = LobbyConfig.POSITION.getValue();
        setTimeRate(0);

        LoaderType worldType = LobbyConfig.WORLD_TYPE.getValue();
        setChunkLoader(worldType.getLoader(this, LobbyConfig.WORLD_NAME.getValue()));

        EventNode<InstanceEvent> eventNode = eventNode();
        eventNode
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    final Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        final Instance instance = player.getInstance();
                        if (instance != null)
                            player.scheduler().scheduleNextTick(() -> onArenaFinish(player));
                        else
                            onFirstSpawn(player);
                    }
                })
                .addListener(ItemDropEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockBreakEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockPlaceEvent.class, event -> event.setCancelled(true));
    }

    void onFirstSpawn(Player player) {
        // TODO: Add first spawn code
    }

    void onArenaFinish(Player player) {
        player.refreshCommands();
        player.getInventory().clear();
        player.teleport(position);
        player.tagHandler().updateContent(NBTCompound.EMPTY);
    }

    public Pos getPosition() {
        return position;
    }
}
