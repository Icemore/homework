package ru.spbau.afanasev.task5;

public class FullPlayerInfo {
    private boolean isAlive;
    private int number;
    private int nextVictim;
    private int roomNumber;

    public FullPlayerInfo(int number) {
        this.number = number;
        isAlive = true;
    }

    public boolean isAlive() {
        return isAlive;
    }

    public void setAlive(boolean isAlive) {
        this.isAlive = isAlive;
    }

    public int getNumber() {
        return number;
    }

    public void setNumber(int number) {
        this.number = number;
    }

    public int getNextVictim() {
        return nextVictim;
    }

    public void setNextVictim(int nextVictim) {
        this.nextVictim = nextVictim;
    }

    public int getRoomNumber() {
        return roomNumber;
    }

    public void setRoomNumber(int roomNumber) {
        this.roomNumber = roomNumber;
    }
}
