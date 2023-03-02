package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.flexegames.feature.arena.BoardFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.state.EndingState;
import me.hsgamer.flexegames.game.pve.state.FightingState;
import me.hsgamer.flexegames.game.pve.state.RestingState;
import me.hsgamer.flexegames.game.pve.state.WaitingState;
import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.entity.Player;
import net.minestom.server.instance.Instance;

import java.util.Collections;
import java.util.List;

public class GameBoardFeature extends BoardFeature {
    private final PveExtension pveExtension;

    public GameBoardFeature(Arena arena, PveExtension pveExtension) {
        super(arena);
        this.pveExtension = pveExtension;
    }

    @Override
    protected Instance getInstance() {
        return arena.getFeature(InstanceFeature.class).getInstance();
    }

    @Override
    protected Component getTitle(Player player) {
        return pveExtension.getMessageConfig().getBoardTitle();
    }

    @Override
    protected List<Component> getLines(Player player) {
        var state = arena.getCurrentState();
        List<Component> components = Collections.emptyList();
        if (state == WaitingState.class) {
            components = pveExtension.getMessageConfig().getBoardLinesWaiting();
        } else if (state == RestingState.class) {
            components = pveExtension.getMessageConfig().getBoardLinesResting();
        } else if (state == FightingState.class) {
            components = pveExtension.getMessageConfig().getBoardLinesFighting();
        } else if (state == EndingState.class) {
            components = pveExtension.getMessageConfig().getBoardLinesEnding();
        }
        return components;
    }
}
