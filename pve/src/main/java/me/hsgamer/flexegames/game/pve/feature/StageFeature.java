package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;

import java.util.concurrent.atomic.AtomicInteger;

public class StageFeature extends ArenaFeature<StageFeature.ArenaStageFeature> {
    @Override
    protected ArenaStageFeature createFeature(Arena arena) {
        return new ArenaStageFeature();
    }

    public static class ArenaStageFeature implements Feature {
        private final AtomicInteger stage = new AtomicInteger(1);

        public int getStage() {
            return stage.get();
        }

        public void setStage(int stage) {
            this.stage.set(stage);
        }

        public void increaseStage() {
            stage.incrementAndGet();
        }
    }
}
