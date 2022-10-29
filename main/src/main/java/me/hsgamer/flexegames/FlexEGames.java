package me.hsgamer.flexegames;

import lombok.Getter;

/**
 * The main class of FlexEGames
 */
public class FlexEGames {
    /**
     * The instance of the game server
     */
    @Getter
    private static GameServer gameServer;

    public static void main(String[] args) {
        gameServer = new GameServer(args);
        gameServer.start();
    }
}
