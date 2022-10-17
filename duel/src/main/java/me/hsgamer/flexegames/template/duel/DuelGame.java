package me.hsgamer.flexegames.template.duel;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.DescriptionFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.flexegames.template.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.template.duel.feature.WinnerFeature;
import me.hsgamer.flexegames.template.duel.state.EndingState;
import me.hsgamer.flexegames.template.duel.state.InGameState;
import me.hsgamer.flexegames.template.duel.state.KillingState;
import me.hsgamer.flexegames.template.duel.state.WaitingState;
import me.hsgamer.flexegames.util.TimeUtil;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.base.GameState;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class DuelGame extends Game {
    private final DuelExtension duelExtension;

    public DuelGame(Pair<GameServer, Config> pair, DuelExtension duelExtension) {
        super(pair);
        this.duelExtension = duelExtension;
    }

    @Override
    public void init() {
        super.init();
        var descriptionFeature = getFeature(DescriptionFeature.class);
        descriptionFeature.setReplacementsFunction(arena -> Map.of(
                "time", () -> Component.text(TimeUtil.format(arena.getArenaFeature(ArenaTimerFeature.class).getDuration(TimeUnit.MILLISECONDS))),
                "winner", () -> Optional.ofNullable(arena.getArenaFeature(WinnerFeature.class).getWinner()).map(Player::getName).orElse(Component.empty()),
                "alive", () -> Component.text(Integer.toString(arena.getArenaFeature(InstanceFeature.class).getAlivePlayers().size()))
        ));
        var joinFeature = getFeature(JoinFeature.class);
        joinFeature.setMaxPlayersFunction(arena -> arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class).getPos().size());
        joinFeature.setPlayersFunction(arena -> arena.getArenaFeature(InstanceFeature.class).getInstance().getPlayers());
        joinFeature.setIsJoinedPredicate((player, arena) -> arena.getArenaFeature(InstanceFeature.class).getInstance() == player.getInstance());
        joinFeature.setJoinResponseFunction((player, arena) -> {
            if (arena.getState() != WaitingState.class) {
                return JoinResponse.fail(duelExtension.getMessageConfig().getNotWaiting());
            }
            var instanceFeature = arena.getArenaFeature(InstanceFeature.class);
            var instance = instanceFeature.getInstance();
            if (instance.getPlayers().size() >= arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class).getPos().size()) {
                return JoinResponse.fail(duelExtension.getMessageConfig().getMaxPlayersReached());
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
                new WaitingState(duelExtension),
                new InGameState(duelExtension),
                new EndingState(duelExtension),
                new KillingState(duelExtension)
        );
    }

    @Override
    protected List<Feature> getFeatures() {
        return List.of(
                new ArenaTimerFeature(),
                new InstanceFeature(),
                new WinnerFeature()
        );
    }
}
