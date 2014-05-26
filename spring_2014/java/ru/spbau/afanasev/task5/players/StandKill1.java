package ru.spbau.afanasev.task5.players;

import ru.spbau.afanasev.task5.Player;
import ru.spbau.afanasev.task5.TournamentInfo;

public class StandKill1 extends Player {

    public StandKill1(int number, TournamentInfo tournamentInfo) {
        super(number, tournamentInfo);
    }

    @Override
    public int move() {
        return 0;
    }

    @Override
    public String getName() {
        return "stand and kill";
    }
}
