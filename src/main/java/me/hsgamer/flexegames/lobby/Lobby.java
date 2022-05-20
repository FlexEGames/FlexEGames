package me.hsgamer.flexegames.lobby;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.modifier.InstanceModifier;
import me.hsgamer.flexegames.board.Board;
import me.hsgamer.flexegames.builder.InstanceModifierBuilder;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.LobbyConfig;
import me.hsgamer.flexegames.config.MessageConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.inventory.Button;
import me.hsgamer.flexegames.inventory.ButtonMap;
import me.hsgamer.flexegames.inventory.ClickConsumer;
import me.hsgamer.flexegames.inventory.RefreshableInventory;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.flexegames.util.FullBrightDimension;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.flexegames.util.TaskUtil;
import me.hsgamer.hscore.common.Validate;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
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
import net.minestom.server.network.packet.server.play.TeamsPacket;
import net.minestom.server.scoreboard.Team;
import net.minestom.server.tag.Tag;
import net.minestom.server.timer.Task;
import net.minestom.server.utils.StringUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;

@ExtensionMethod({ItemUtil.class})
public class Lobby extends InstanceContainer {
    private final Pos position;
    private final Board board;
    private final Task boardTask;
    private final Team lobbyTeam;
    private final GameServer gameServer;
    private final List<InstanceModifier> instanceModifiers;
    private final Tag<Boolean> hidePlayerTag = Tag.Boolean("lobbyHidePlayer").defaultValue(false);

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
                            player.scheduler().scheduleNextTick(() -> onBackSpawn(player));
                        else
                            onFirstSpawn(player);
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
                .schedule();
        lobbyTeam = MinecraftServer.getTeamManager().createBuilder("lobbyTeam")
                .collisionRule(TeamsPacket.CollisionRule.NEVER)
                .build();

        registerHotbarItemFromMap(LobbyConfig.HOTBAR_CREATOR.getValue(), 1, this::openTemplateInventory);
        registerHotbarItemFromMap(LobbyConfig.HOTBAR_SELECTOR.getValue(), 4, player -> openArenaInventory(player, false));
        registerHotbarItemFromMap(LobbyConfig.HOTBAR_TOGGLE_PLAYER.getValue(), 7, player -> {
            if (Boolean.TRUE.equals(player.getTag(hidePlayerTag))) {
                player.sendMessage(MessageConfig.LOBBY_SHOW_PLAYERS.getValue());
                player.updateViewerRule(entity -> true);
                player.setTag(hidePlayerTag, false);
            } else {
                player.sendMessage(MessageConfig.LOBBY_HIDE_PLAYERS.getValue());
                player.updateViewerRule(entity -> !(entity instanceof Player));
                player.setTag(hidePlayerTag, true);
            }
        });

        instanceModifiers = new ArrayList<>();
        LobbyConfig.MODIFIERS.getValue()
                .forEach(map -> InstanceModifierBuilder.buildInstanceModifier(map)
                        .map(provider -> provider.getInstanceModifier(this))
                        .ifPresent(instanceModifiers::add)
                );
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

    private void onFirstSpawn(Player player) {
        board.addPlayer(player);
        player.setRespawnPoint(position);
        player.setGameMode(GameMode.ADVENTURE);
        player.setTeam(lobbyTeam);
    }

    private void onBackSpawn(Player player) {
        onFirstSpawn(player);
        player.teleport(position);
    }

    private void onQuit(Player player) {
        board.removePlayer(player);
        player.setTeam(null);
        player.removeTag(hidePlayerTag);
        player.updateViewerRule(entity -> true);
    }

    public Pos getPosition() {
        return position;
    }

    public void init() {
        instanceModifiers.forEach(InstanceModifier::init);
    }

    public void clear() {
        boardTask.cancel();
        instanceModifiers.forEach(InstanceModifier::clear);
    }

    public boolean isInLobby(Player player) {
        return player.getInstance() == null || player.getInstance() == this;
    }

    public void openTemplateInventory(Player openPlayer) {
        var templates = new ArrayList<>(gameServer.getTemplateManager().getTemplateMap().values());
        var maxPage = getMaxPage(templates.size());
        var pages = new AtomicReference<>(0);
        Button nextPageButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_TEMPLATE_NEXT_PAGE.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    var page = pages.get();
                    if (page < maxPage - 1) {
                        pages.set(page + 1);
                    }
                    return true;
                };
            }
        };
        Button previousPageButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_TEMPLATE_PREVIOUS_PAGE.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    var page = pages.get();
                    if (page > 0) {
                        pages.set(page - 1);
                    }
                    return true;
                };
            }
        };
        Button arenaButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_TEMPLATE_ARENA.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    openArenaInventory(player, false);
                    return false;
                };
            }
        };
        Button dummyButton = () -> ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty());
        Button airButton = () -> ItemStack.AIR;
        ButtonMap buttonMap = inventory -> {
            var buttons = new HashMap<Integer, Button>();
            var page = pages.get();
            for (int i = 0; i < 18; i++) {
                var index = i + page * 18;
                if (index >= templates.size()) {
                    buttons.put(i, airButton);
                    continue;
                }
                var template = templates.get(index);
                buttons.put(i, new Button() {
                    @Override
                    public ItemStack getItem() {
                        return template.getDisplayItem().stripItalics();
                    }

                    @Override
                    public ClickConsumer getClickConsumer() {
                        return (player, clickType, result) -> {
                            result.setCancel(true);
                            if (gameServer.getGameArenaManager().createArena(player, template)) {
                                player.sendMessage(MessageConfig.RESPONSE_CREATE_ARENA_SUCCESSFUL.getValue());
                                openArenaInventory(player, true);
                            } else {
                                player.sendMessage(MessageConfig.RESPONSE_CANNOT_CREATE_ARENA.getValue());
                            }
                            return false;
                        };
                    }
                });
            }
            buttons.put(18, previousPageButton);
            buttons.put(19, nextPageButton);
            for (int i = 20; i < 26; i++) {
                buttons.put(i, dummyButton);
            }
            buttons.put(26, arenaButton);
            return buttons;
        };
        RefreshableInventory inventory = RefreshableInventory.builder()
                .setInventoryType(InventoryType.CHEST_3_ROW)
                .setTitle(LobbyConfig.INVENTORY_TEMPLATE_TITLE.getValue())
                .setButtonMap(buttonMap)
                .setUnregisterOnClose(true)
                .build();
        openPlayer.openInventory(inventory);
    }

    public void openArenaInventory(Player openPlayer, Supplier<List<Arena>> arenaSupplier) {
        var arenaSupplierRef = new AtomicReference<>(arenaSupplier);
        var arenas = new AtomicReference<>(arenaSupplierRef.get().get());
        var pages = new AtomicReference<>(0);

        Button nextPageButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_NEXT_PAGE.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    var page = pages.get() + 1;
                    if (page >= getMaxPage(arenas.get().size())) {
                        page = 0;
                    }
                    pages.set(page);
                    return true;
                };
            }
        };
        Button previousPageButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_PREVIOUS_PAGE.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    var page = pages.get() - 1;
                    if (page < 0) {
                        page = Math.max(getMaxPage(arenas.get().size()) - 1, 0);
                    }
                    pages.set(page);
                    return true;
                };
            }
        };
        Button globalArenaButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_GLOBAL_ARENA.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    pages.set(0);
                    arenaSupplierRef.set(() -> gameServer.getGameArenaManager().getAllArenas());
                    return true;
                };
            }
        };
        Button myArenaButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_MY_ARENA.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    pages.set(0);
                    arenaSupplierRef.set(() -> gameServer.getGameArenaManager().findArenasByOwner(player));
                    return true;
                };
            }
        };
        Button templateButton = new Button() {
            @Override
            public ItemStack getItem() {
                return ItemBuilder.buildItem(LobbyConfig.INVENTORY_ARENA_TEMPLATE.getValue()).stripItalics();
            }

            @Override
            public ClickConsumer getClickConsumer() {
                return (player, clickType, result) -> {
                    result.setCancel(true);
                    openTemplateInventory(player);
                    return false;
                };
            }
        };
        Button dummyButton = () -> ItemStack.of(Material.BLACK_STAINED_GLASS_PANE).withDisplayName(Component.empty());
        Button airButton = () -> ItemStack.AIR;
        ButtonMap buttonMap = inventory -> {
            var buttons = new HashMap<Integer, Button>();
            var arenasList = arenas.get();
            var page = pages.get();
            for (int i = 0; i < 18; i++) {
                var index = i + page * 18;
                if (index >= arenasList.size()) {
                    buttons.put(i, airButton);
                    continue;
                }
                var arena = arenasList.get(index);
                var feature = arena.getArenaFeature(GameFeature.class);
                if (feature.getGame() == null) {
                    buttons.put(i, airButton);
                    continue;
                }
                buttons.put(i, new Button() {
                    @Override
                    public ItemStack getItem() {
                        return feature.getGame().getDisplayItem().stripItalics();
                    }

                    @Override
                    public ClickConsumer getClickConsumer() {
                        return (player, clickType, result) -> {
                            result.setCancel(true);
                            var response = feature.joinGame(player);
                            if (!response.success()) {
                                player.sendMessage(response.getMessage(player));
                            } else {
                                player.closeInventory();
                            }
                            return false;
                        };
                    }
                });
            }
            buttons.put(18, previousPageButton);
            buttons.put(19, nextPageButton);
            buttons.put(20, dummyButton);
            buttons.put(21, dummyButton);
            buttons.put(22, dummyButton);
            buttons.put(23, dummyButton);
            buttons.put(24, myArenaButton);
            buttons.put(25, globalArenaButton);
            buttons.put(26, templateButton);
            return buttons;
        };
        RefreshableInventory inventory = RefreshableInventory.builder()
                .setInventoryType(InventoryType.CHEST_3_ROW)
                .setTitle(LobbyConfig.INVENTORY_ARENA_TITLE.getValue())
                .setButtonMap(buttonMap)
                .setRefreshHandler((inv, firstTime) -> {
                    arenas.set(arenaSupplierRef.get().get());
                    return true;
                })
                .setUnregisterOnClose(true)
                .setUpdateSchedule(TaskUtil.tick(10))
                .build();
        openPlayer.openInventory(inventory);
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
