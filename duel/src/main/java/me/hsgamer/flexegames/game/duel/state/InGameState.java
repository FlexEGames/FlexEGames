package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentGameState;
import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.DuelGameConfig;
import me.hsgamer.flexegames.game.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.game.duel.feature.WinnerFeature;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Player;

import java.util.ArrayList;

public class InGameState implements ComponentGameState {
    private final DuelExtension duelExtension;

    public InGameState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void start(Arena arena) {
        var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(DuelGameConfig.class);
        var joinFeature = arena.getArenaFeature(JoinFeature.class);
        var players = new ArrayList<>(joinFeature.getPlayers());
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            Pos pos = gameConfig.getPos().get(i % gameConfig.getPos().size());
            player.teleport(pos);
        }
        arena.getArenaFeature(InstanceFeature.class).giveKit();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getArenaFeature(WinnerFeature.class).checkWinner()) {
            arena.setNextState(EndingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateInGame();
    }
}
