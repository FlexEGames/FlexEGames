package me.hsgamer.flexegames.lobby;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.Template;
import me.hsgamer.flexegames.api.modifier.InstanceModifier;
import me.hsgamer.flexegames.builder.InstanceModifierBuilder;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.LobbyConfig;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.*;
import me.hsgamer.hscore.common.Validate;
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
import net.minestom.server.event.Event;
import net.minestom.server.event.EventListener;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.instance.RemoveEntityFromInstanceEvent;
import net.minestom.server.event.inventory.InventoryPreClickEvent;
import net.minestom.server.event.item.ItemDropEvent;
import net.minestom.server.event.player.*;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
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
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
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
    private final Map<UUID, Supplier<List<Arena>>> arenaSupplierRefMap = new ConcurrentHashMap<>();
    private GUIHolder arenaGUIHolder;
    private GUIHolder templateGUIHolder;

    public Lobby(GameServer gameServer) {
        super(LobbyConfig.UNIQUE_ID.getValue(), FullBrightDimension.INSTANCE);
        this.gameServer = gameServer;
        position = LobbyConfig.POSITION.getValue();
        board = new Board(
                player -> ReplacementManager.builder().replaceGlobal().replacePlayer(player).build(LobbyConfig.BOARD_TITLE.getValue()),
                player -> {
                    ReplacementManager.Builder builder = ReplacementManager.builder().replaceGlobal().replacePlayer(player);
                    return LobbyConfig.BOARD_LINES.getValue().stream()
                            .map(builder::build)
                            .toList();
                }
        );
        setTimeRate(0);

        var worldType = LobbyConfig.WORLD_TYPE.getValue();
        setChunkLoader(worldType.getLoader(this, AssetUtil.getWorldFile(LobbyConfig.WORLD_NAME.getValue()).toPath()));

        EventNode<InstanceEvent> eventNode = eventNode();
        eventNode
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        Instance instance = player.getInstance();
                        if (instance != null)
                            player.scheduler().scheduleNextTick(() -> onBack(player));
                    }
                })
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
                .addListener(PlayerSwapItemEvent.class, event -> event.setCancelled(true))
                .addListener(InventoryPreClickEvent.class, event -> {
                    if (event.getInventory() == null) {
                        event.setCancelled(true);
                    }
                });
        boardTask = scheduler().buildTask(board::updateAll)
                .repeat(TaskUtil.tick(LobbyConfig.BOARD_UPDATE_TIME.getValue()))
                .executionType(Boolean.TRUE.equals(LobbyConfig.BOARD_ASYNC.getValue()) ? ExecutionType.ASYNC : ExecutionType.SYNC)
                .schedule();

        registerHotbarItemFromMap(LobbyConfig.HOTBAR_CREATOR.getValue(), 2, this::openTemplateInventory);
        registerHotbarItemFromMap(LobbyConfig.HOTBAR_SELECTOR.getValue(), 4, player -> openArenaInventory(player, false));
        registerHotbarItemFromMap(LobbyConfig.HOTBAR_TOGGLE_PLAYER.getValue(), 6, player -> {
            player.setTag(hidePlayerTag, !player.getTag(hidePlayerTag));
            updateView(player, true);
        });

        var hubHotbarMap = LobbyConfig.HOTBAR_SERVER_HUB.getValue();
        var hubName = Objects.toString(hubHotbarMap.get("server"), "hub");
        registerHotbarItemFromMap(hubHotbarMap, 8, player -> {
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
        LobbyConfig.MODIFIERS.getValue()
                .forEach(map -> InstanceModifierBuilder.buildInstanceModifier(map)
                        .map(provider -> provider.getInstanceModifier(this))
                        .ifPresent(instanceModifiers::add)
                );

        setupArenaGUIHolder();
        setupTemplateGUIHolder();
    }

    public void registerHotbarItemFromMap(Map<String, Object> map, int defaultSlot, Consumer<Player> consumer) {
        var item = ItemBuilder.buildItem(map).stripItalics();
        var slot = Optional.ofNullable(map.get("slot")).map(Objects::toString).flatMap(Validate::getNumber).map(BigDecimal::intValue).orElse(defaultSlot);
        boolean enable = Optional.ofNullable(map.get("enable")).map(Objects::toString).map(Boolean::parseBoolean).orElse(true);
        if (enable) {
            registerHotbarItem(slot, item, consumer);
        }
    }

    public void registerHotbarItem(int slot, ItemStack itemStack, Consumer<Player> consumer) {
        eventNode()
                .addListener(EventListener.builder(RemoveEntityFromInstanceEvent.class)
                        .handler(event -> ((Player) event.getEntity()).getInventory().setItemStack(slot, ItemStack.AIR))
                        .filter(event -> event.getEntity() instanceof Player)
                        .build())
                .addListener(EventListener.builder(AddEntityToInstanceEvent.class)
                        .handler(event -> {
                            Player player = (Player) event.getEntity();
                            player.scheduler().scheduleNextTick(() -> player.getInventory().setItemStack(slot, itemStack));
                        })
                        .filter(event -> event.getEntity() instanceof Player)
                        .build())
                .addListener(EventListener.builder(ItemDropEvent.class)
                        .handler(event -> event.setCancelled(true))
                        .filter(event -> event.getItemStack().equals(itemStack))
                        .build())
                .addListener(EventListener.builder(PlayerUseItemEvent.class)
                        .handler(event -> {
                            event.setCancelled(true);
                            consumer.accept(event.getPlayer());
                        })
                        .filter(event -> event.getPlayer().getInventory().getItemInHand(event.getHand()).equals(itemStack))
                        .build())
                .addListener(EventListener.builder(PlayerBlockInteractEvent.class)
                        .handler(event -> {
                            event.setCancelled(true);
                            event.setBlockingItemUse(true);
                            consumer.accept(event.getPlayer());
                        })
                        .filter(event -> event.getPlayer().getInventory().getItemInHand(event.getHand()).equals(itemStack))
                        .build())
                .addListener(EventListener.builder(PlayerHandAnimationEvent.class)
                        .handler(event -> {
                            event.setCancelled(true);
                            consumer.accept(event.getPlayer());
                        })
                        .filter(event -> event.getPlayer().getInventory().getItemInHand(event.getHand()).equals(itemStack))
                        .build());
    }

    public void hook(EventNode<Event> node) {
        node.addListener(PlayerSpawnEvent.class, event -> {
            var player = event.getPlayer();
            if (player.getInstance() == this) {
                onSpawn(player);
            }
        });
        node.addListener(PlayerLoginEvent.class, event -> {
            event.setSpawningInstance(this);
            var player = event.getPlayer();
            player.setRespawnPoint(position);
            player.setGameMode(GameMode.ADVENTURE);
        });
    }

    private void updateView(Player player, boolean message) {
        if (Boolean.TRUE.equals(player.getTag(hidePlayerTag))) {
            if (message) player.sendMessage(MessageConfig.LOBBY_HIDE_PLAYERS.getValue());
            player.updateViewerRule(entity -> !(entity instanceof Player));
        } else {
            if (message) player.sendMessage(MessageConfig.LOBBY_SHOW_PLAYERS.getValue());
            player.updateViewerRule(entity -> true);
        }
    }

    private void onSpawn(Player player) {
        board.addPlayer(player);
    }

    private void onBack(Player player) {
        player.teleport(position);
        player.setRespawnPoint(position);
        player.setGameMode(GameMode.ADVENTURE);
        updateView(player, false);
    }

    private void onQuit(Player player) {
        board.removePlayer(player);
        player.setTeam(null);
        player.updateViewerRule(entity -> true);
    }

    public Pos getPosition() {
        return position;
    }

    public void init() {
        instanceModifiers.forEach(InstanceModifier::init);
    }

    public void clear() {
        if (templateGUIHolder != null) {
            templateGUIHolder.stop();
        }
        if (arenaGUIHolder != null) {
            arenaGUIHolder.stop();
        }
        boardTask.cancel();
        instanceModifiers.forEach(InstanceModifier::clear);
    }

    public boolean isInLobby(Player player) {
        return player.getInstance() == null || player.getInstance() == this;
    }

    private void setupTemplateGUIHolder() {
        templateGUIHolder = new GUIHolder();
        Supplier<List<Template>> templates = () -> new ArrayList<>(gameServer.getTemplateManager().getTemplateMap().values());
        IntSupplier maxPage = () -> getMaxPage(templates.get().size());
        var uuidPage = new HashMap<UUID, Integer>();
        Button nextPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_TEMPLATE_NEXT_PAGE.getValue()).stripItalics();
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
                templateGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button previousPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_TEMPLATE_PREVIOUS_PAGE.getValue()).stripItalics();
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
                templateGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button arenaButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_TEMPLATE_ARENA.getValue()).stripItalics();
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
                if (index >= templates.get().size()) {
                    continue;
                }
                var template = templates.get().get(index);
                buttons.put(new Button() {
                    @Override
                    public ItemStack getItemStack(UUID uuid) {
                        return template.getDisplayItem().stripItalics();
                    }

                    @Override
                    public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                        var player = uuid.getPlayer();
                        if (gameServer.getGameArenaManager().createArena(player, template)) {
                            player.sendMessage(MessageConfig.RESPONSE_CREATE_ARENA_SUCCESSFUL.getValue());
                            openArenaInventory(player, true);
                        } else {
                            player.sendMessage(MessageConfig.RESPONSE_CANNOT_CREATE_ARENA.getValue());
                        }
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
        templateGUIHolder.setTitle(LobbyConfig.INVENTORY_TEMPLATE_TITLE.getValue());
        templateGUIHolder.setInventoryType(InventoryType.CHEST_3_ROW);
        templateGUIHolder.setRemoveDisplayOnClose(true);
        templateGUIHolder.setButtonMap(buttonMap);
        templateGUIHolder.init();
    }

    public void openTemplateInventory(Player openPlayer) {
        templateGUIHolder.createDisplay(openPlayer.getUuid()).init();
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
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_NEXT_PAGE.getValue()).stripItalics();
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
                templateGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button previousPageButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_PREVIOUS_PAGE.getValue()).stripItalics();
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
                templateGUIHolder.getDisplay(uuid).ifPresent(GUIDisplay::update);
                return false;
            }
        };
        Button globalArenaButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_GLOBAL_ARENA.getValue()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.put(uuid, 0);
                setArenaSupplierRef(uuid, () -> gameServer.getGameArenaManager().getAllArenas());
                return false;
            }
        };
        Button myArenaButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_MY_ARENA.getValue()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                uuidPage.put(uuid, 0);
                setArenaSupplierRef(uuid, () -> gameServer.getGameArenaManager().findArenasByOwner(uuid1 -> uuid1.equals(uuid)));
                return false;
            }
        };
        Button templateButton = new Button() {
            @Override
            public ItemStack getItemStack(UUID uuid) {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_TEMPLATE.getValue()).stripItalics();
            }

            @Override
            public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                openTemplateInventory(uuid.getPlayer());
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
                var feature = arena.getArenaFeature(GameFeature.class);
                if (!feature.isReady()) {
                    continue;
                }
                buttons.put(new Button() {
                    @Override
                    public ItemStack getItemStack(UUID uuid) {
                        return feature.getGame().getDisplayItem().stripItalics();
                    }

                    @Override
                    public boolean handleAction(UUID uuid, InventoryPreClickEvent event) {
                        var player = uuid.getPlayer();
                        var response = feature.joinGame(player);
                        if (!response.success()) {
                            player.sendMessage(response.getMessage(player));
                        } else {
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
            buttons.put(templateButton, Collections.singletonList(26));
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
        arenaGUIHolder.setTitle(LobbyConfig.INVENTORY_ARENA_TITLE.getValue());
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
        openArenaInventory(openPlayer, () -> gameServer.getGameArenaManager().findArenasByOwner(uuids));
    }

    public void openArenaInventory(Player openPlayer, boolean myArena) {
        if (myArena) {
            openArenaInventory(openPlayer, () -> gameServer.getGameArenaManager().findArenasByOwner(openPlayer));
        } else {
            openArenaInventory(openPlayer, () -> gameServer.getGameArenaManager().getAllArenas());
        }
    }

    private int getMaxPage(int size) {
        return size / 18 + (size % 18 == 0 ? 0 : 1);
    }
}
