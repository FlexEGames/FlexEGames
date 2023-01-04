package me.hsgamer.flexegames.state;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.game.ComponentDisplayName;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;

/**
 * The killing state. This will remove the {@link Arena} from {@link me.hsgamer.flexegames.manager.GameArenaManager}
 */
public class KillingState implements GameState, ComponentDisplayName {
    private final GameServer gameServer;

    public KillingState(GameServer gameServer) {
        this.gameServer = gameServer;
    }

    @Override
    public Component getDisplayNameAsComponent() {
        return gameServer.getMessageConfig().getStateKilling();
    }

    @Override
    public void update(Arena arena) {
        arena.removeFromManager();
    }
}
