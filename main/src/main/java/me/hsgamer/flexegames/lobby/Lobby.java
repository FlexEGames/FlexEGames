package me.hsgamer.flexegames.lobby;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.api.modifier.InstanceModifier;
import me.hsgamer.flexegames.builder.InstanceModifierBuilder;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.*;
import me.hsgamer.hscore.minestom.board.Board;
import me.hsgamer.hscore.minestom.gui.GUIDisplay;
import me.hsgamer.hscore.minestom.gui.GUIHolder;
import me.hsgamer.hscore.minestom.gui.button.Button;
import me.hsgamer.hscore.minestom.gui.button.ButtonMap;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.inventory.InventoryPreClickEvent;
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

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.IntSupplier;
import java.util.function.Supplier;

@ExtensionMethod({ItemUtil.class, PlayerUtil.class})
public class Lobby extends InstanceContainer {
    private final Pos position;
    private final Board board;
    private final Task boardTask;
    private final GameServer gameServer;
    private final List<InstanceModifier> instanceModifiers;
    private final Tag<Boolean> hidePlayerTag = Tag.Boolean("lobby:HidePlayer").defaultValue(false);
    private final Tag<Boolean> firstSpawnTag = Tag.Boolean("lobby:FirstSpawn").defaultValue(true);
    private final Map<UUID, Supplier<List<Arena>>> arenaSupplierRefMap = new ConcurrentHashMap<>();
    private final HotbarItemsHelper hotbarItemsHelper;
    private GUIHolder arenaGUIHolder;
    private GUIHolder gameGUIHolder;

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

        var worldType = gameServer.getLobbyConfig().getWorldType();
        setChunkLoader(worldType.getLoader(this, AssetUtil.getWorldFile(gameServer.getLobbyConfig().getWorldName()).toPath()));

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

    public Pos getPosition() {
        return position;
    }

    public HotbarItemsHelper getHotbarItemsHelper() {
        return hotbarItemsHelper;
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
        gameGUIHolder = new GUIHolder();
        Supplier<List<Game>> games = () -> new ArrayList<>(gameServer.getGameManager().getGameMap().values());
        IntSupplier maxPage = () -> getMaxPage(games.get().size());
        var uuidPage = new HashMap<UUID, Integer>();
        Button nextPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getGameInventoryNextPage()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.compute(uuid, (key, oldPage) -> {
                    if (oldPage == null)
                        oldPage = 0;
                    if (oldPage < maxPage.getAsInt() - 1) {
                        return oldPage + 1;
                    } else {
                        return oldPage;
                    }
                });
                gameGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button previousPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getGameInventoryPreviousPage()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.compute(uuid, (key, oldPage) -> {
                    if (oldPage == null)
                        oldPage = 0;
                    if (oldPage > 0) {
                        return oldPage - 1;
                    } else {
                        return oldPage;
                    }
                });
                gameGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button arenaButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getGameInventoryArena()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                openArenaInventory(uuid.getPlayer(), false);
                return false;
            }
        };
        Button dummyButton = uuid -> ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty());
        ButtonMap buttonMap = uuid -> {
            var buttons = new HashMap<Button, List<Integer>>();
            var page = uuidPage.getOrDefault(uuid, 0);
            for (int i = 0; i < 18; i++) {
                var index = i + page * 18;
                if (index >= games.get().size()) {
                    continue;
                }
                var game = games.get().get(index);
                buttons.put(new Button() {
                    @Override
                    public ItemStack getItemStack(UUID uuid) {
                        return game.getFeature(DescriptionFeature.class).getDisplayItem();
                    }

                    @Override
                    public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                        var player = uuid.getPlayer();
                        game.createArena(player.getUuid());
                        openArenaInventory(player, false);
                        return false;
                    }
                }, Collections.singletonList(i));
            }
            buttons.put(previousPageButton, Collections.singletonList(18));
            buttons.put(nextPageButton, Collections.singletonList(19));
            buttons.put(dummyButton, Arrays.asList(20, 21, 22, 23, 24, 25));
            buttons.put(arenaButton, Collections.singletonList(26));
            return buttons;
        };
        gameGUIHolder.setTitle(gameServer.getLobbyConfig().getGameInventoryTitle());
        gameGUIHolder.setInventoryType(InventoryType.CHEST_3_ROW);
        gameGUIHolder.setRemoveDisplayOnClose(true);
        gameGUIHolder.setButtonMap(buttonMap);
        gameGUIHolder.init();
    }

    public void openGameInventory(Player openPlayer) {
        gameGUIHolder.createDisplay(openPlayer.getUuid()).init();
    }

    public void setArenaSupplierRef(UUID uuid, Supplier<List<Arena>> arenaSupplierRef) {
        arenaSupplierRefMap.put(uuid, arenaSupplierRef);
    }

    private void setupArenaGUIHolder() {
        var uuidArenas = new ConcurrentHashMap<UUID, List<Arena>>();
        var uuidPage = new ConcurrentHashMap<UUID, Integer>();
        Button nextPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryNextPage()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.compute(uuid, (key, oldPage) -> {
                    if (oldPage == null)
                        oldPage = 0;
                    if (oldPage < getMaxPage(uuidArenas.getOrDefault(uuid, Collections.emptyList()).size()) - 1) {
                        return oldPage + 1;
                    } else {
                        return oldPage;
                    }
                });
                gameGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button previousPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryPreviousPage()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.compute(uuid, (key, oldPage) -> {
                    if (oldPage == null)
                        oldPage = 0;
                    if (oldPage > 0) {
                        return oldPage - 1;
                    } else {
                        return oldPage;
                    }
                });
                gameGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button globalArenaButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryGlobalArena()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.put(uuid, 0);
                setArenaSupplierRef(uuid, () -> gameServer.getGameManager().getAllArenas());
                return false;
            }
        };
        Button myArenaButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryMyArena()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.put(uuid, 0);
                setArenaSupplierRef(uuid, () -> gameServer.getGameManager().findArenasByOwner(uuid1 -> uuid1.equals(uuid)));
                return false;
            }
        };
        Button gameButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(gameServer.getLobbyConfig().getArenaInventoryGame()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                openGameInventory(uuid.getPlayer());
                return false;
            }
        };
        Button dummyButton = uuid -> ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty());
        ButtonMap buttonMap = uuid -> {
            var buttons = new HashMap<Button, List<Integer>>();
            var arenasList = uuidArenas.getOrDefault(uuid, Collections.emptyList());
            var page = uuidPage.getOrDefault(uuid, 0);
            for (int i = 0; i < 18; i++) {
                var index = i + page * 18;
                if (index >= arenasList.size()) {
                    continue;
                }
                var arena = arenasList.get(index);
                buttons.put(new Button() {
                    @Override
                    public ItemStack getItemStack(UUID uuid) {
                        return arena.getArenaFeature(DescriptionFeature.class).getDisplayItem();
                    }

                    @Override
                    public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                        var player = uuid.getPlayer();
                        if (tryJoinArena(player, arena)) {
                            player.closeInventory();
                        }
                        return false;
                    }
                }, Collections.singletonList(i));
            }
            buttons.put(previousPageButton, Collections.singletonList(18));
            buttons.put(nextPageButton, Collections.singletonList(19));
            buttons.put(dummyButton, Arrays.asList(20, 21, 22, 23));
            buttons.put(myArenaButton, Collections.singletonList(24));
            buttons.put(globalArenaButton, Collections.singletonList(25));
            buttons.put(gameButton, Collections.singletonList(26));
            return buttons;
        };


        var updateTasks = new ConcurrentHashMap<UUID, Task>();
        arenaGUIHolder = new GUIHolder() {
            @Override
            public GUIDisplay createDisplay(UUID uuid) {
                GUIDisplay guiDisplay = super.createDisplay(uuid);
                updateTasks.put(uuid, MinecraftServer.getSchedulerManager().scheduleTask(() -> {
                    uuidArenas.put(uuid, arenaSupplierRefMap.get(uuid).get());
                    guiDisplay.update();
                }, TaskSchedule.nextTick(), TaskSchedule.nextTick()));
                return guiDisplay;
            }

            @Override
            public void removeDisplay(UUID uuid) {
                super.removeDisplay(uuid);
                var task = updateTasks.get(uuid);
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

    public void openArenaInventory(Player openPlayer, Supplier<List<Arena>> arenaSupplier) {
        arenaSupplierRefMap.put(openPlayer.getUuid(), arenaSupplier);
        arenaGUIHolder.createDisplay(openPlayer.getUuid()).init();
    }

    public void openArenaInventory(Player openPlayer, String ownerQuery) {
        List<UUID> uuids = MinecraftServer.getConnectionManager().getOnlinePlayers()
                .stream()
                .filter(player -> StringUtils.jaroWinklerScore(player.getUsername().toLowerCase(), ownerQuery.toLowerCase()) > 0)
                .map(Player::getUuid)
                .toList();
        openArenaInventory(openPlayer, () -> gameServer.getGameManager().findArenasByOwner(uuids));
    }

    public void openArenaInventory(Player openPlayer, boolean myArena) {
        if (myArena) {
            openArenaInventory(openPlayer, () -> gameServer.getGameManager().findArenasByOwner(openPlayer));
        } else {
            openArenaInventory(openPlayer, () -> gameServer.getGameManager().getAllArenas());
        }
    }

    public boolean tryJoinArena(Player player, Arena arena) {
        var joinFeature = arena.getArenaFeature(JoinFeature.class);
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

    private int getMaxPage(int size) {
        return size / 18 + (size % 18 == 0 ? 0 : 1);
    }
}
