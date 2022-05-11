package me.hsgamer.flexegames;

import lombok.Getter;

public class FlexEGames {
    @Getter
    private static final GameServer gameServer = new GameServer();

    public static void main(String[] args) {
        gameServer.start();
    }
}
