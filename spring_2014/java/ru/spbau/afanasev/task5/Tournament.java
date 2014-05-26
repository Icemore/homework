package ru.spbau.afanasev.task5;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class Tournament implements Runnable {
    private final int[] moves;
    private List<IPlayer> players;
    private TournamentInfo tournamentInfo;
    private int playersAlive;
    private int playersMoved;
    private int canMoveFlag;

    private List<Integer> finalTable;

    public Tournament(int fieldSize, List<Class<? extends Player>> players, List<Integer> finalTable) {
        this.finalTable = finalTable;
        this.players = new ArrayList<>();
        this.tournamentInfo = new TournamentInfo(fieldSize, players.size());
        this.playersAlive = players.size();
        this.moves = new int[players.size()];
        this.canMoveFlag = 1;

        finalTable.clear();
        for (int i = 0; i < players.size(); i++) {
            finalTable.add(0);
            try {
                Constructor<? extends Player> constructor = players.get(i).getConstructor(int.class, TournamentInfo.class);
                this.players.add(constructor.newInstance(i, tournamentInfo));
            } catch (NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
                kill(i);
            }
        }

        initPlayers();
    }

    private void initPlayers() {
        Random random = new Random();

        // set rooms
        for (IPlayer player : players) {
            int room = random.nextInt(tournamentInfo.getFieldSize());
            tournamentInfo.getPlayerInfo(player.getNumber()).setRoomNumber(room);
        }

        // set victims
        List<Integer> victimList = new ArrayList<>();
        for (int i = 0; i < players.size(); i++) {
            victimList.add(i);
        }

        Collections.shuffle(victimList, random);

        int last = victimList.get(0);
        for (int i = 1; i < victimList.size(); i++) {
            tournamentInfo.getPlayerInfo(last).setNextVictim(victimList.get(i));
            last = victimList.get(i);
        }

        tournamentInfo.getPlayerInfo(last).setNextVictim(victimList.get(0));
    }

    private void kill(int number) {
        FullPlayerInfo info = tournamentInfo.getPlayerInfo(number);
        if (info.isAlive()) {
            info.setAlive(false);
            playersAlive -= 1;
        }
    }

    private void kill(int number, int killerNumber) {
        FullPlayerInfo info = tournamentInfo.getPlayerInfo(number);
        tournamentInfo.getPlayerInfo(killerNumber).setNextVictim(info.getNextVictim());
        kill(number);
        addPoints(killerNumber, 2);
    }

    private void killall() {
        for (IPlayer player : players) {
            kill(player.getNumber());
        }
    }

    private void addPoints(int number, int amount) {
        finalTable.set(number, finalTable.get(number) + amount);
    }

    public void makeMove(int number, int move) {
        synchronized (moves) {
            moves[number] = move;
            playersMoved++;

            if (playersMoved == playersAlive) moves.notify();
        }
    }

    public int canMove() {
        return canMoveFlag;
    }

    public void processMoves() {
        // process kills
        for (IPlayer player : players) {
            if (!player.isAlive()) continue;

            int number = player.getNumber();
            if (moves[number] == 0) {
                FullPlayerInfo info = tournamentInfo.getPlayerInfo(number);
                FullPlayerInfo enemy = tournamentInfo.getPlayerInfo(info.getNextVictim());

                if (info.getRoomNumber() == enemy.getRoomNumber()) {
                    kill(enemy.getNumber(), number);
                }
            }
        }

        //process moves
        for (IPlayer player : players) {
            if (!player.isAlive()) continue;

            int number = player.getNumber();
            if (moves[number] == -1 || moves[number] == 1) {
                FullPlayerInfo info = tournamentInfo.getPlayerInfo(number);
                int newRoomNumber = info.getRoomNumber() + moves[number];

                if (newRoomNumber >= 0 && newRoomNumber < tournamentInfo.getFieldSize()) {
                    info.setRoomNumber(newRoomNumber);
                }
            }
        }
    }

    private void rewardWinner() {
        IPlayer winner = null;

        for (IPlayer player : players) {
            if (player.isAlive()) {
                winner = player;
                break;
            }
        }

        if (winner != null)
            addPoints(winner.getNumber(), 5);
    }

    @Override
    public void run() {
        Thread[] playerThreads = new Thread[players.size()];

        for (int i = 0; i < players.size(); i++) {
            playerThreads[i] = new Thread(new PlayerRunner(this, players.get(i)));
            playerThreads[i].start();
        }

        while (playersAlive > 1) {
            playersMoved = 0;

            synchronized (this) {
                canMoveFlag *= -1;
                this.notifyAll();
            }

            synchronized (moves) {
                while (playersMoved != playersAlive) try {
                    moves.wait();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }

            processMoves();
        }

        rewardWinner();

        killall();
        synchronized (this) {
            canMoveFlag *= -1;
            this.notifyAll();
        }

        for (Thread thread : playerThreads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
