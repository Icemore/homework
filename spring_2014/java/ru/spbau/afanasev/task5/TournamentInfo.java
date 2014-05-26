package ru.spbau.afanasev.task5;

import java.util.ArrayList;
import java.util.List;

public class TournamentInfo {
    private int fieldSize;
    private FullPlayerInfo[] playersInfo;

    public TournamentInfo(int fieldSize, int playersCnt) {
        this.fieldSize = fieldSize;

        playersInfo = new FullPlayerInfo[playersCnt];
        for (int i = 0; i < playersCnt; i++) {
            playersInfo[i] = new FullPlayerInfo(i);
        }
    }

    public FullPlayerInfo getPlayerInfo(int number) {
        return playersInfo[number];
    }

    public int getFieldSize() {
        return fieldSize;
    }

    public List<PlayerInfo> getPartialPlayersInfo() {
        List<PlayerInfo> res = new ArrayList<>();

        for (FullPlayerInfo info : playersInfo) {
            if (info.isAlive()) {
                res.add(new PlayerInfo(info));
            }
        }

        return res;
    }
}
