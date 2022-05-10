package me.hsgamer.flexgames;

import lombok.Getter;

public class FlexGames {
    @Getter
    private static final GameServer gameServer = new GameServer();

    public static void main(String[] args) {
        gameServer.start();
    }
}
