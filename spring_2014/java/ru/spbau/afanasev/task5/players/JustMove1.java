package ru.spbau.afanasev.task5.players;

import ru.spbau.afanasev.task5.Player;
import ru.spbau.afanasev.task5.TournamentInfo;

public class JustMove1 extends Player {
    private int direction;

    public JustMove1(int number, TournamentInfo tournamentInfo) {
        super(number, tournamentInfo);
        direction = 1;
    }

    @Override
    public int move() {
        if (getRoomNumber() == 0) direction = 1;
        if (getRoomNumber() == getFieldSize() - 1) direction = -1;

        return direction;
    }

    @Override
    public String getName() {
        return "just move";
    }
}
