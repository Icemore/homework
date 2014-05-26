package ru.spbau.afanasev.task5;

import java.io.FileNotFoundException;

public class Main {
    public static void main(String[] args) {
        String playersDir = "src\\ru\\spbau\\afanasev\\task5\\players";
        int fieldSize = 10;
        int tournamentsCnt = 5;

        try {
            Game game = new Game(fieldSize, playersDir, tournamentsCnt);
            game.run();
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
        } catch (BadPlayerClassException e) {
            System.err.println("Bad player class: " + e.getMessage());
        } catch (InterruptedException ignored) {
        }
    }
}
