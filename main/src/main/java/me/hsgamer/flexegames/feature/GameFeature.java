package me.hsgamer.flexegames.feature;

import lombok.Getter;
import lombok.Setter;
import me.hsgamer.flexegames.api.game.ArenaGame;
import me.hsgamer.flexegames.api.game.JoinResponse;
import me.hsgamer.flexegames.api.game.Template;
import me.hsgamer.flexegames.util.ArenaUtil;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import net.minestom.server.entity.Player;

import java.util.UUID;

public class GameFeature extends ArenaFeature<GameFeature.ArenaGameFeature> {

    @Override
    protected ArenaGameFeature createFeature(Arena arena) {
        return new ArenaGameFeature(arena);
    }

    public static class ArenaGameFeature implements Feature {
        private final Arena arena;
        @Getter
        private ArenaGame game;
        @Getter
        @Setter
        private UUID owner;
        @Getter
        @Setter
        private boolean isFailed = false;

        public ArenaGameFeature(Arena arena) {
            this.arena = arena;
        }

        public void setGame(Template template) {
            this.game = template.createGame(arena);
        }

        public boolean isReady() {
            return game != null;
        }

        public JoinResponse joinGame(Player player) {
            if (game == null) {
                return JoinResponse.INCOMPLETE_SETUP;
            }
            var event = ArenaUtil.callPreJoinEvent(arena, player);
            if (!event.getResponse().success()) {
                return event.getResponse();
            }
            return game.join(player);
        }
    }
}
