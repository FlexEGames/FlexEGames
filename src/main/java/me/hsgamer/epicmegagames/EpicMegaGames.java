package me.hsgamer.epicmegagames;

import lombok.Getter;

public class EpicMegaGames {
    @Getter
    private static final GameServer gameServer = new GameServer();

    public static void main(String[] args) {
        gameServer.start();
    }
}
