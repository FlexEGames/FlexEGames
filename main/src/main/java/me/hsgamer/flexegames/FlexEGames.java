package me.hsgamer.flexegames;

import lombok.Getter;

public class FlexEGames {
    @Getter
    private static final GameServer gameServer = new GameServer();

    public static void main(String[] args) {
        System.setProperty("minestom.packet-queue-size", "9999");
        System.setProperty("minestom.packet-per-tick", "100");
        gameServer.start();
    }
}
