package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.game.pve.feature.InstanceFeature;
import me.hsgamer.flexegames.game.pve.feature.MobGeneratorFeature;
import me.hsgamer.flexegames.game.pve.feature.StageFeature;
import me.hsgamer.flexegames.game.pve.state.*;
import me.hsgamer.flexegames.util.TimeUtil;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class PveGame extends Game {
    public static final int SPAWN_RADIUS = 10;
    public static final int HEIGHT = 16;
    private final PveExtension pveExtension;

    public PveGame(Pair<GameServer, Config> pair, PveExtension pveExtension) {
        super(pair);
        this.pveExtension = pveExtension;
    }

    @Override
    public void init() {
        super.init();
        var descriptionFeature = getFeature(DescriptionFeature.class);
        descriptionFeature.setReplacementsFunction(arena -> Map.of(
                "time", () -> Component.text(TimeUtil.format(arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS))),
                "stage", () -> Component.text(Integer.toString(arena.getArenaFeature(StageFeature.class).getStage()))
        ));
        var joinFeature = getFeature(JoinFeature.class);
        joinFeature.setMaxPlayersFunction(arena -> arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getMaxPlayers());
        joinFeature.setPlayersFunction(arena -> arena.getArenaFeature(InstanceFeature.class).getInstance().getPlayers());
        joinFeature.setIsJoinedPredicate((player, arena) -> arena.getArenaFeature(InstanceFeature.class).getInstance() == player.getInstance());
        joinFeature.setJoinResponseFunction((player, arena) -> {
            if (arena.getState() != WaitingState.class) {
                return JoinResponse.fail(pveExtension.getMessageConfig().getNotWaiting());
            }
            var instanceFeature = arena.getArenaFeature(InstanceFeature.class);
            var instance = instanceFeature.getInstance();
            if (instance.getPlayers().size() >= arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getMaxPlayers()) {
                return JoinResponse.fail(pveExtension.getMessageConfig().getMaxPlayersReached());
            }
            player.setInstance(instance);
            return JoinResponse.successful();
        });
    }

    @Override
    protected Class<? extends GameState> getInitialState() {
        return WaitingState.class;
    }

    @Override
    protected List<GameState> loadGameStates() {
        return List.of(
                new WaitingState(pveExtension),
                new RestingState(pveExtension),
                new FightingState(pveExtension),
                new EndingState(pveExtension),
                new KillingState(pveExtension)
        );
    }

    @Override
    protected List<Feature> getFeatures() {
        return List.of(
                new ArenaTimerFeature(),
                new InstanceFeature(),
                new StageFeature(),
                new MobGeneratorFeature()
        );
    }
}
