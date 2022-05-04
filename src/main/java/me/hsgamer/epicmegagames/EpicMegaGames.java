package me.hsgamer.epicmegagames;

public class EpicMegaGames {
    private static final GameServer gameServer = new GameServer();

    public static void main(String[] args) {
        gameServer.enable();
        gameServer.start();
        gameServer.disable();
    }

    public static GameServer getGameServer() {
        return gameServer;
    }
}
