package me.hsgamer.flexegames.game.pve;

import me.hsgamer.flexegames.GameServer;
import me.hsgamer.flexegames.api.extension.SingleGameExtension;
import me.hsgamer.flexegames.game.Game;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.hscore.config.Config;

import java.util.function.Function;

public class PvEExtension extends SingleGameExtension {
    @Override
    public Function<Pair<GameServer, Config>, Game> getInitializer() {
        return PveGame::new;
    }

    @Override
    public String[] getId() {
        return new String[]{"pve"};
    }
}
