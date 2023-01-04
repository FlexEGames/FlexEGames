package me.hsgamer.flexegames.game.duel.state;

import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.feature.ConfigFeature;
import me.hsgamer.flexegames.game.duel.feature.InstanceFeature;
import me.hsgamer.flexegames.game.duel.feature.WinnerFeature;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Player;

import java.util.ArrayList;

public class InGameState implements GameState, ComponentDisplayName {
    private final DuelExtension duelExtension;

    public InGameState(DuelExtension duelExtension) {
        this.duelExtension = duelExtension;
    }

    @Override
    public void start(Arena arena) {
        var gameConfig = arena.getFeature(ConfigFeature.class).config();
        var joinFeature = arena.getFeature(JoinFeature.class);
        var players = new ArrayList<>(joinFeature.getPlayers());
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            Pos pos = gameConfig.getPos().get(i % gameConfig.getPos().size());
            player.teleport(pos);
        }
        arena.getFeature(InstanceFeature.class).giveKit();
    }

    @Override
    public void update(Arena arena) {
        if (arena.getFeature(WinnerFeature.class).checkWinner()) {
            arena.setNextState(EndingState.class);
        }
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return duelExtension.getMessageConfig().getStateInGame();
    }
}
