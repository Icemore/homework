package ru.spbau.afanasev.task5;

import java.util.List;

public interface IPlayer {
    int move();

    int getNumber();

    int getRoomNumber();

    int getNextVictim();

    boolean isAlive();

    List<PlayerInfo> getPlayersInfo();

    String getName();
}
