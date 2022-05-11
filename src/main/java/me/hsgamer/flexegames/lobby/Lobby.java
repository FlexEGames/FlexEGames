package me.hsgamer.flexegames.lobby;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.board.Board;
import me.hsgamer.flexegames.builder.ItemBuilder;
import me.hsgamer.flexegames.config.LobbyConfig;
import me.hsgamer.flexegames.feature.GameFeature;
import me.hsgamer.flexegames.inventory.Button;
import me.hsgamer.flexegames.inventory.ButtonMap;
import me.hsgamer.flexegames.inventory.ClickConsumer;
import me.hsgamer.flexegames.inventory.RefreshableInventory;
import me.hsgamer.flexegames.manager.ReplacementManager;
import me.hsgamer.flexegames.util.FullBrightDimension;
import me.hsgamer.flexegames.util.ItemUtil;
import me.hsgamer.flexegames.util.TaskUtil;
import me.hsgamer.hscore.common.Validate;
import net.kyori.adventure.text.Component;
import net.minestom.server.MinecraftServer;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.GameMode;
import net.minestom.server.entity.Player;
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
import net.minestom.server.timer.Task;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

@ExtensionMethod({ItemUtil.class})
public class Lobby extends InstanceContainer {
    private final Pos position;
    private final Board board;
    private final Task boardTask;
    private final Team lobbyTeam;
    private final GameServer gameServer;

    public Lobby(GameServer gameServer) {
        super(UUID.randomUUID(), FullBrightDimension.INSTANCE);
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
        setChunkLoader(worldType.getLoader(this, LobbyConfig.WORLD_NAME.getValue()));

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
                .addListener(ItemDropEvent.class, event -> event.setCancelled(true))
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

        var selectorMap = LobbyConfig.HOTBAR_SELECTOR.getValue();
        var selectorItem = ItemBuilder.buildItem(selectorMap);
        var selectorSlot = Validate.getNumber(Objects.toString(selectorMap.getOrDefault("slot", 0))).map(BigDecimal::intValue).orElse(4);
        registerHotbarItem(selectorSlot, selectorItem, player -> openArenaInventory(player, false));
    }

    public void registerHotbarItem(int slot, ItemStack itemStack, Consumer<Player> consumer) {
        eventNode()
                .addListener(ItemDropEvent.class, event -> {
                    if (event.getItemStack().equals(itemStack)) {
                        event.setCancelled(true);
                    }
                })
                .addListener(AddEntityToInstanceEvent.class, event -> {
                    Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        player.scheduler().scheduleNextTick(() -> player.getInventory().setItemStack(slot, itemStack));
                    }
                })
                .addListener(RemoveEntityFromInstanceEvent.class, event -> {
                    Entity entity = event.getEntity();
                    if (entity instanceof Player player) {
                        player.getInventory().setItemStack(slot, ItemStack.AIR);
                    }
                })
                .addListener(PlayerUseItemEvent.class, event -> {
                    event.setCancelled(true);
                    if (event.getItemStack().equals(itemStack)) {
                        consumer.accept(event.getPlayer());
                    }
                })
                .addListener(PlayerBlockInteractEvent.class, event -> {
                    if (event.getHand() != Player.Hand.MAIN) {
                        return;
                    }
                    if (event.getPlayer().getInventory().getItemInHand(event.getHand()).equals(itemStack)) {
                        event.setCancelled(true);
                        consumer.accept(event.getPlayer());
                    }
                });
    }

    private void onFirstSpawn(Player player) {
        board.addPlayer(player);
        player.setRespawnPoint(position);
        player.setEnableRespawnScreen(false);
        player.setGameMode(GameMode.ADVENTURE);
        player.setTeam(lobbyTeam);
    }

    private void onBackSpawn(Player player) {
        board.addPlayer(player);
        player.setGameMode(GameMode.ADVENTURE);
        player.heal();
        player.setFood(20);
        player.setLevel(0);
        player.setAllowFlying(false);
        player.setFlying(false);
        player.setInvisible(false);
        player.setAdditionalHearts(0);
        player.clearEffects();
        player.setNoGravity(false);
        player.setExp(0);
        player.stopSpectating();
        player.askSynchronization();
        player.teleport(position);
        player.setRespawnPoint(position);
        player.setEnableRespawnScreen(false);
        player.setTeam(lobbyTeam);
    }

    private void onQuit(Player player) {
        board.removePlayer(player);
        player.setTeam(null);
    }

    public Pos getPosition() {
        return position;
    }

    public void clear() {
        boardTask.cancel();
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
                            var arena = gameServer.getGameArenaManager().createNewArena();
                            arena.getArenaFeature(GameFeature.class).setGame(template);
                            arena.getArenaFeature(GameFeature.class).setOwner(player.getUuid());
                            openArenaInventory(player, true);
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
                .build()
                .unregisterWhenClosed();
        openPlayer.openInventory(inventory);
    }

    public void openArenaInventory(Player openPlayer, boolean myArena) {
        var arenas = new AtomicReference<>(gameServer.getGameArenaManager().getArenas(openPlayer, myArena));
        var isMyArena = new AtomicBoolean(myArena);
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
                    isMyArena.set(false);
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
                    isMyArena.set(true);
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
                var game = arena.getArenaFeature(GameFeature.class).getGame();
                if (game == null) {
                    buttons.put(i, airButton);
                    continue;
                }
                buttons.put(i, new Button() {
                    @Override
                    public ItemStack getItem() {
                        return game.getDisplayItem().stripItalics();
                    }

                    @Override
                    public ClickConsumer getClickConsumer() {
                        return (player, clickType, result) -> {
                            result.setCancel(true);
                            var response = game.join(player);
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
                    arenas.set(gameServer.getGameArenaManager().getArenas(openPlayer, isMyArena.get()));
                    return true;
                })
                .build()
                .autoRefresh(TaskUtil.tick(20))
                .unregisterWhenClosed();
        openPlayer.openInventory(inventory);
    }

    private int getMaxPage(int size) {
        return size / 18 + (size % 18 == 0 ? 0 : 1);
    }
}
