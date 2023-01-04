package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.minigamecore.base.Feature;

import java.util.concurrent.atomic.AtomicInteger;

public class StageFeature implements Feature {
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
