package ru.spbau.afanasev.task5;

import java.util.List;

public abstract class Player implements IPlayer {
    private int number;
    private TournamentInfo tournamentInfo;

    protected Player(int number, TournamentInfo tournamentInfo) {
        this.number = number;
        this.tournamentInfo = tournamentInfo;
    }

    @Override
    public int getNumber() {
        return number;
    }

    @Override
    public int getRoomNumber() {
        return tournamentInfo.getPlayerInfo(number).getRoomNumber();
    }

    @Override
    public int getNextVictim() {
        return tournamentInfo.getPlayerInfo(number).getNextVictim();
    }

    @Override
    public List<PlayerInfo> getPlayersInfo() {
        return tournamentInfo.getPartialPlayersInfo();
    }

    @Override
    public boolean isAlive() {
        return tournamentInfo.getPlayerInfo(number).isAlive();
    }

    protected int getFieldSize() {
        return tournamentInfo.getFieldSize();
    }
}
