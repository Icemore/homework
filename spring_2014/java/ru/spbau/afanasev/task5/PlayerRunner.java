package ru.spbau.afanasev.task5;

public class PlayerRunner implements Runnable {
    private final Tournament tournament;
    private IPlayer player;

    public PlayerRunner(Tournament tournament, IPlayer player) {
        this.tournament = tournament;
        this.player = player;
    }

    @Override
    public void run() {
        int expectFlag = -1;

        while (true) {
            synchronized (tournament) {
                while (tournament.canMove() != expectFlag) try {
                    tournament.wait();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return;
                }
                expectFlag *= -1;
            }

            if (!player.isAlive()) return;

            tournament.makeMove(player.getNumber(), player.move());
        }
    }
}
