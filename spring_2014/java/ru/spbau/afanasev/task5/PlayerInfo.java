package ru.spbau.afanasev.task5;

public class PlayerInfo {
    private int roomNumber;
    private int number;

    public PlayerInfo(int roomNumber, int number) {
        this.roomNumber = roomNumber;
        this.number = number;
    }

    public PlayerInfo(FullPlayerInfo info) {
        this(info.getRoomNumber(), info.getNumber());
    }

    public int getRoomNumber() {
        return roomNumber;
    }

    public int getNumber() {
        return number;
    }
}
