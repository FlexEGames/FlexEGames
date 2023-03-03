package me.hsgamer.flexegames.game.duel.feature;

import me.hsgamer.flexegames.feature.arena.BoardFeature;
import me.hsgamer.flexegames.game.duel.DuelExtension;
import me.hsgamer.flexegames.game.duel.state.EndingState;
import me.hsgamer.flexegames.game.duel.state.InGameState;
import me.hsgamer.flexegames.game.duel.state.WaitingState;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.instance.Instance;

import java.util.Collections;
import java.util.List;

public class GameBoardFeature extends BoardFeature {
    private final DuelExtension duelExtension;

    public GameBoardFeature(Arena arena, DuelExtension duelExtension) {
        super(arena);
        this.duelExtension = duelExtension;
    }

    @Override
    protected Instance getInstance() {
        return arena.getFeature(InstanceFeature.class).getInstance();
    }

    @Override
    protected Component getTitle(Player player) {
        return duelExtension.getMessageConfig().getBoardTitle();
    }

    @Override
    protected List<Component> getLines(Player player) {
        List<Component> components = Collections.emptyList();
        var state = arena.getCurrentState();
        if (state == WaitingState.class) {
            components = duelExtension.getMessageConfig().getBoardLinesWaiting();
        } else if (state == InGameState.class) {
            components = duelExtension.getMessageConfig().getBoardLinesIngame();
        } else if (state == EndingState.class) {
            components = duelExtension.getMessageConfig().getBoardLinesEnding();
        }
        return components;
    }
}
