package me.hsgamer.flexegames.lobby;

import lombok.Getter;
import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.Game;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.api.modifier.InstanceModifier;
import me.hsgamer.flexegames.builder.ChunkLoaderBuilder;
import me.hsgamer.flexegames.builder.InstanceModifierBuilder;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.feature.arena.DescriptionFeature;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.manager.GameArenaManager;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.*;
import me.hsgamer.hscore.minecraft.gui.GUIDisplay;
import me.hsgamer.hscore.minecraft.gui.advanced.AdvancedButtonMap;
import me.hsgamer.hscore.minecraft.gui.button.Button;
import me.hsgamer.hscore.minecraft.gui.button.impl.SimpleButton;
import me.hsgamer.hscore.minecraft.gui.mask.MaskUtils;
import me.hsgamer.hscore.minecraft.gui.mask.impl.ButtonMapMask;
import me.hsgamer.hscore.minecraft.gui.mask.impl.ButtonPaginatedMask;
import me.hsgamer.hscore.minestom.board.Board;
import me.hsgamer.hscore.minestom.gui.MinestomGUIDisplay;
import me.hsgamer.hscore.minestom.gui.MinestomGUIHolder;
import me.hsgamer.hscore.minestom.gui.object.MinestomItem;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.player.*;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.InstanceContainer;
import net.minestom.server.inventory.InventoryType;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;
import net.minestom.server.tag.Tag;
import net.minestom.server.timer.ExecutionType;
import net.minestom.server.timer.Task;
import net.minestom.server.timer.TaskSchedule;
import net.minestom.server.utils.StringUtils;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * The lobby
 */
@ExtensionMethod({ItemUtil.class, PlayerUtil.class})
public class Lobby extends InstanceContainer {
    @Getter
    private final Pos position;
    private final Board board;
    private final Task boardTask;
    private final GameServer gameServer;
    private final List<InstanceModifier> instanceModifiers;
    private final Tag<Boolean> hidePlayerTag = Tag.Boolean("lobby:HidePlayer").defaultValue(false);
    private final Tag<Boolean> firstSpawnTag = Tag.Boolean("lobby:FirstSpawn").defaultValue(true);
    private final Map<UUID, Supplier<List<Arena>>> arenaSupplierRefMap = new ConcurrentHashMap<>();
    @Getter
    private final HotbarItemsHelper hotbarItemsHelper;
    private MinestomGUIHolder arenaGUIHolder;
    private MinestomGUIHolder gameGUIHolder;

    public Lobby(GameServer gameServer) {
        super(gameServer.getLobbyConfig().getWorldId(), FullBrightDimension.INSTANCE);
        this.gameServer = gameServer;
        this.hotbarItemsHelper = new HotbarItemsHelper(this);
        position = gameServer.getLobbyConfig().getWorldSpawnPos();
        board = new Board(
                player -> ReplacementManager.builder().replaceGlobal().replacePlayer(player).build(gameServer.getLobbyConfig().getBoardTitle()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder().replaceGlobal().replacePlayer(player);
                    return gameServer.getLobbyConfig().getBoardLines().stream()
                            .map(builder::build)
                            .toList();
                }
        );
        setTimeRate(0);
        setTime(6000);

        EventNode<InstanceEvent> eventNode = eventNode();
        eventNode
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        onQuit(player);
                    }
                })
                .addListener(PlayerMoveEvent.class, event -> {
                    if (isInVoid(event.getNewPosition())) {
                        event.setNewPosition(position);
                    }
                })
                .addListener(PlayerBlockBreakEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerBlockPlaceEvent.class, event -> event.setCancelled(true))
                .addListener(PlayerSwapItemEvent.class, event -> event.setCancelled(true));
        ChatUtil.apply(eventNode, gameServer.getLobbyConfig().getChatFormat());
        boardTask = scheduler().buildTask(board::updateAll)
                .repeat(TaskUtil.tick(gameServer.getLobbyConfig().getBoardUpdateInterval()))
                .executionType(gameServer.getLobbyConfig().isBoardAsync() ? ExecutionType.ASYNC : ExecutionType.SYNC)
                .schedule();

        hotbarItemsHelper.registerHotbarItemFromMap(gameServer.getLobbyConfig().getGameHotbar(), 2, this::openGameInventory);
        hotbarItemsHelper.registerHotbarItemFromMap(gameServer.getLobbyConfig().getArenaHotbar(), 4, player -> openArenaInventory(player, false));
        hotbarItemsHelper.registerHotbarItemFromMap(gameServer.getLobbyConfig().getTogglePlayerHotbar(), 6, player -> {
            player.setTag(hidePlayerTag, !player.getTag(hidePlayerTag));
            updateView(player, true);
        });

        var hubHotbarMap = gameServer.getLobbyConfig().getServerHubHotbar();
        var hubName = Objects.toString(hubHotbarMap.get("server"), "hub");
        hotbarItemsHelper.registerHotbarItemFromMap(hubHotbarMap, 8, player -> {
            try (
                    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                    DataOutputStream dataOutputStream = new DataOutputStream(byteArrayOutputStream)
            ) {
                dataOutputStream.writeUTF("Connect");
                dataOutputStream.writeUTF(hubName);
                player.sendPluginMessage("bungeecord:main", byteArrayOutputStream.toByteArray());
            } catch (IOException e) {
                MinecraftServer.LOGGER.warn("There is an exception when sending data", e);
            }
        });

        instanceModifiers = new ArrayList<>();
        gameServer.getLobbyConfig().getWorldModifiers()
                .forEach(map -> InstanceModifierBuilder.buildInstanceModifier(map)
                        .map(provider -> provider.getInstanceModifier(this))
                        .ifPresent(instanceModifiers::add)
                );

        setupArenaGUIHolder();
        setupGameGUIHolder();
    }

    private void updateView(Player player, boolean message) {
        if (Boolean.TRUE.equals(player.getTag(hidePlayerTag))) {
            if (message) player.sendMessage(gameServer.getMessageConfig().getLobbyHidePlayers());
            player.updateViewerRule(entity -> !(entity instanceof Player));
        } else {
            if (message) player.sendMessage(gameServer.getMessageConfig().getLobbyShowPlayers());
            player.updateViewerRule(entity -> true);
        }
    }

    public void init() {
        var worldType = gameServer.getLobbyConfig().getWorldType();
        ChunkLoaderBuilder.INSTANCE.build(worldType, this, AssetUtil.getWorldFile(gameServer.getLobbyConfig().getWorldName()).toPath()).ifPresent(this::setChunkLoader);

        instanceModifiers.forEach(InstanceModifier::init);
        var node = MinecraftServer.getGlobalEventHandler();
        node.addListener(PlayerSpawnEvent.class, event -> {
            var player = event.getPlayer();
            if (player.getInstance() == this) {
                onSpawn(player);
            }
        });
        node.addListener(PlayerDisconnectEvent.class, event -> onDisconnect(event.getPlayer()));
        node.addListener(PlayerLoginEvent.class, event -> {
            event.setSpawningInstance(this);
            var player = event.getPlayer();
            player.setRespawnPoint(position);
            player.setGameMode(GameMode.ADVENTURE);
        });
        hotbarItemsHelper.init();
        hotbarItemsHelper.setEnabled(true);
    }

    private void onSpawn(Player player) {
        if (Boolean.TRUE.equals(player.getTag(firstSpawnTag))) {
            onFirstSpawn(player);
            player.setTag(firstSpawnTag, false);
        } else {
            onBackSpawn(player);
        }
    }

    private void onFirstSpawn(Player player) {
        board.addPlayer(player);
    }

    private void onBackSpawn(Player player) {
        PlayerUtil.reset(player);
        player.setRespawnPoint(position);
        player.setGameMode(GameMode.ADVENTURE);
        updateView(player, false);
        onFirstSpawn(player);
        player.teleport(position);
    }

    private void onQuit(Player player) {
        board.removePlayer(player);
        player.updateViewerRule(entity -> true);
    }

    private void onDisconnect(Player player) {
        player.removeTag(hidePlayerTag);
        player.removeTag(firstSpawnTag);
    }

    public void clear() {
        if (gameGUIHolder != null) {
            gameGUIHolder.stop();
        }
        if (arenaGUIHolder != null) {
            arenaGUIHolder.stop();
        }
        boardTask.cancel();
        instanceModifiers.forEach(InstanceModifier::clear);
        hotbarItemsHelper.clear();
    }

    public boolean isInLobby(Player player) {
        return player.getInstance() == null || player.getInstance() == this;
    }

    private void setupGameGUIHolder() {
        gameGUIHolder = new MinestomGUIHolder();
        Supplier<List<Game>> games = () -> new ArrayList<>(gameServer.getGameManager().getGameMap().values());
        AdvancedButtonMap buttonMap = new AdvancedButtonMap();

        ButtonPaginatedMask gamesMask = new ButtonPaginatedMask("games", MaskUtils.generateAreaSlots(0, 0, 8, 1).boxed().toList()) {
            @Override
            public @NotNull List<@NotNull Button> getButtons(@NotNull UUID uuid) {
                return games.get().stream()
                        .<Button>map(game -> new SimpleButton(new MinestomItem(game.getDisplayItem()), event -> {
                            var player = event.getViewerID().getPlayer();
                            gameServer.getGameManager().createArena(player, game).whenComplete((arenaCreated, throwable) -> {
                                if (throwable != null) {
                                    MinecraftServer.LOGGER.error("There is an exception when creating arena", throwable);
                                    return;
                                }
                                if (arenaCreated) {
                                    openArenaInventory(player, false);
                                }
                            });
                        }))
                        .toList();
            }
        };

        Button nextPageButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getGameInventoryNextPage()).stripItalics().asMinestomItem(), event -> {
            gamesMask.nextPage(event.getViewerID());
            gameGUIHolder.getDisplay(event.getViewerID()).ifPresent(GUIDisplay::update);
        });
        Button previousPageButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getGameInventoryPreviousPage()).stripItalics().asMinestomItem(), event -> {
            gamesMask.previousPage(event.getViewerID());
            gameGUIHolder.getDisplay(event.getViewerID()).ifPresent(GUIDisplay::update);
        });
        Button arenaButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getGameInventoryArena()).stripItalics().asMinestomItem(), event -> openArenaInventory(event.getViewerID().getPlayer(), false));
        Button dummyButton = uuid -> ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty()).asMinestomItem();
        ButtonMapMask actionMask = new ButtonMapMask("action")
                .addButton(previousPageButton, 18)
                .addButton(nextPageButton, 19)
                .addButton(dummyButton, 20, 21, 22, 23, 24, 25)
                .addButton(arenaButton, 26);

        buttonMap.addMask(gamesMask);
        buttonMap.addMask(actionMask);
        gameGUIHolder.setTitle(gameServer.getLobbyConfig().getGameInventoryTitle());
        gameGUIHolder.setInventoryType(InventoryType.CHEST_3_ROW);
        gameGUIHolder.setRemoveDisplayOnClose(true);
        gameGUIHolder.setButtonMap(buttonMap);
        gameGUIHolder.init();
    }

    /**
     * Open the game inventory for the player
     *
     * @param openPlayer the player to open the inventory for
     */
    public void openGameInventory(Player openPlayer) {
        gameGUIHolder.createDisplay(openPlayer.getUuid()).open();
    }

    /**
     * Set the arena supplier for the unique id
     *
     * @param uuid             the unique id
     * @param arenaSupplierRef the arena supplier
     */
    public void setArenaSupplierRef(UUID uuid, Supplier<List<Arena>> arenaSupplierRef) {
        arenaSupplierRefMap.put(uuid, arenaSupplierRef);
    }

    private void setupArenaGUIHolder() {
        var uuidArenas = new ConcurrentHashMap<UUID, List<Arena>>();
        AdvancedButtonMap buttonMap = new AdvancedButtonMap();

        ButtonPaginatedMask arenasMask = new ButtonPaginatedMask("arenas", MaskUtils.generateAreaSlots(0, 0, 8, 1).boxed().toList()) {
            @Override
            public @NotNull List<@NotNull Button> getButtons(@NotNull UUID uuid) {
                return uuidArenas.getOrDefault(uuid, Collections.emptyList())
                        .stream()
                        .filter(GameArenaManager::isValid)
                        .<Button>map(arena -> new SimpleButton(arena.getFeature(DescriptionFeature.class).getDisplayItem().asMinestomItem(), event -> {
                            var player = event.getViewerID().getPlayer();
                            if (tryJoinArena(player, arena)) {
                                player.closeInventory();
                            }
                        }))
                        .toList();
            }
        };

        Button nextPageButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryNextPage()).stripItalics().asMinestomItem(), event -> arenasMask.nextPage(event.getViewerID()));
        Button previousPageButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryPreviousPage()).stripItalics().asMinestomItem(), event -> arenasMask.previousPage(event.getViewerID()));
        Button globalArenaButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryGlobalArena()).stripItalics().asMinestomItem(), event -> {
            arenasMask.setPage(event.getViewerID(), 0);
            setArenaSupplierRef(event.getViewerID(), () -> gameServer.getArenaManager().getAllArenas());
        });
        Button myArenaButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryMyArena()).stripItalics().asMinestomItem(), event -> {
            arenasMask.setPage(event.getViewerID(), 0);
            setArenaSupplierRef(event.getViewerID(), () -> gameServer.getArenaManager().findArenasByOwner(uuid1 -> uuid1.equals(event.getViewerID())));
        });
        Button gameButton = new SimpleButton(ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryGame()).stripItalics().asMinestomItem(), event -> openGameInventory(event.getViewerID().getPlayer()));
        Button dummyButton = uuid -> ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty()).asMinestomItem();
        ButtonMapMask actionMask = new ButtonMapMask("action")
                .addButton(previousPageButton, 18)
                .addButton(nextPageButton, 19)
                .addButton(dummyButton, 20, 21, 22, 23)
                .addButton(myArenaButton, 24)
                .addButton(globalArenaButton, 25)
                .addButton(gameButton, 26);
        buttonMap.addMask(arenasMask);
        buttonMap.addMask(actionMask);

        var updateTasks = new ConcurrentHashMap<UUID, Task>();
        arenaGUIHolder = new MinestomGUIHolder() {
            @Override
            public @NotNull MinestomGUIDisplay newDisplay(UUID uuid) {
                MinestomGUIDisplay guiDisplay = super.newDisplay(uuid);
                updateTasks.put(uuid, MinecraftServer.getSchedulerManager().scheduleTask(() -> {
                    uuidArenas.put(uuid, arenaSupplierRefMap.get(uuid).get());
                    guiDisplay.update();
                }, TaskSchedule.nextTick(), TaskSchedule.nextTick()));
                return guiDisplay;
            }

            @Override
            protected void onRemoveDisplay(@NotNull MinestomGUIDisplay display) {
                super.onRemoveDisplay(display);
                var task = updateTasks.get(display.getUniqueId());
                if (task != null) {
                    task.cancel();
                }
            }
        };
        arenaGUIHolder.setButtonMap(buttonMap);
        arenaGUIHolder.setTitle(gameServer.getLobbyConfig().getArenaInventoryTitle());
        arenaGUIHolder.setInventoryType(InventoryType.CHEST_3_ROW);
        arenaGUIHolder.setRemoveDisplayOnClose(true);
        arenaGUIHolder.init();
    }

    /**
     * Open the arena inventory for the player
     *
     * @param openPlayer    the player to open the inventory for
     * @param arenaSupplier the arena supplier
     */
    public void openArenaInventory(Player openPlayer, Supplier<List<Arena>> arenaSupplier) {
        arenaSupplierRefMap.put(openPlayer.getUuid(), arenaSupplier);
        arenaGUIHolder.createDisplay(openPlayer.getUuid()).open();
    }

    /**
     * Open the arena inventory for the player
     *
     * @param openPlayer the player to open the inventory for
     * @param ownerQuery the query to filter the owner of the arena
     */
    public void openArenaInventory(Player openPlayer, String ownerQuery) {
        List<UUID> uuids = MinecraftServer.getConnectionManager().getOnlinePlayers()
                .stream()
                .filter(player -> StringUtils.jaroWinklerScore(player.getUsername().toLowerCase(), ownerQuery.toLowerCase()) > 0)
                .map(Player::getUuid)
                .toList();
        openArenaInventory(openPlayer, () -> gameServer.getArenaManager().findArenasByOwner(uuids));
    }

    /**
     * Open the arena inventory for the player
     *
     * @param openPlayer the player to open the inventory for
     * @param myArena    whether to filter the arena by the open player
     */
    public void openArenaInventory(Player openPlayer, boolean myArena) {
        if (myArena) {
            openArenaInventory(openPlayer, () -> gameServer.getArenaManager().findArenasByOwner(openPlayer));
        } else {
            openArenaInventory(openPlayer, () -> gameServer.getArenaManager().getAllArenas());
        }
    }

    /**
     * Try to join the arena
     *
     * @param player the player to join the arena
     * @param arena  the arena to join
     * @return whether the player is successfully joined the arena
     */
    public boolean tryJoinArena(Player player, Arena arena) {
        var joinFeature = arena.getFeature(JoinFeature.class);
        if (joinFeature.isJoined(player)) {
            player.sendMessage(gameServer.getMessageConfig().getErrorArenaJoined());
            return false;
        } else {
            JoinResponse response = joinFeature.join(player);
            if (response.success()) {
                return true;
            } else {
                player.sendMessage(response.message());
                return false;
            }
        }
    }
}
