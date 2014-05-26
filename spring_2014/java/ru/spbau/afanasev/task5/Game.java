package ru.spbau.afanasev.task5;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;


public class Game {
    private int fieldSize;
    private int tournamentsCnt;
    private List<Class<? extends Player>> playerClasses;

    public Game(int fieldSize, String playersFolder, int tournamentsCnt) throws FileNotFoundException, BadPlayerClassException {
        this.fieldSize = fieldSize;
        this.tournamentsCnt = tournamentsCnt;

        playerClasses = new ArrayList<>();
        loadClasses(playersFolder);
    }

    @SuppressWarnings("unchecked")
    private void loadClasses(String playersFolder) throws FileNotFoundException, BadPlayerClassException {
        File classDir = new File(playersFolder);
        URL url;
        try {
            url = classDir.toURI().toURL();
        } catch (MalformedURLException e) {
            throw new FileNotFoundException();
        }

        URLClassLoader classLoader = new URLClassLoader(new URL[]{url});
        for (File file : classDir.listFiles()) {
            try {

                Class<?> cur = classLoader.loadClass("ru.spbau.afanasev.task5.players." + file.getName().substring(0, file.getName().lastIndexOf('.')));

                if (!Player.class.isAssignableFrom(cur)) {
                    throw new BadPlayerClassException("Class " + cur.getName() + "does not extends Player");
                }

                playerClasses.add((Class<? extends Player>) cur);
            } catch (ClassNotFoundException e) {
                throw new BadPlayerClassException();
            }
        }
    }

    public void run() throws InterruptedException {
        Thread[] tournamentThreads = new Thread[tournamentsCnt];

        List<List<Integer>> finalTable = new ArrayList<>();
        for (int i = 0; i < tournamentsCnt; i++) {
            finalTable.add(new ArrayList<Integer>());
            tournamentThreads[i] = new Thread(new Tournament(fieldSize, playerClasses, finalTable.get(i)));
            tournamentThreads[i].start();
        }

        for (int i = 0; i < tournamentsCnt; i++) {
            tournamentThreads[i].join();
        }

        printTable(finalTable);
    }

    private void printTable(List<List<Integer>> finalTable) {
        for (int i = 0; i < playerClasses.size(); i++) {
            System.out.printf("%10s", playerClasses.get(i).getName());

            int sum = 0;
            for (int j = 0; j < tournamentsCnt; j++) {
                int cur = finalTable.get(j).get(i);
                System.out.printf("%5d", cur);
                sum += cur;
            }

            System.out.printf("%5d", sum);
            System.out.println();
        }
    }
}
