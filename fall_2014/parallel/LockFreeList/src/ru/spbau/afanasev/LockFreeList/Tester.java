package ru.spbau.afanasev.LockFreeList;

import java.util.ArrayList;

public class Tester {
    private Set<Integer> set;
    private int numberOfReaders;
    private int numberOfWriters;
    private int numberOfOperations;

    public Tester(Set<Integer> set, int numberOfReaders, int numberOfWriters, int numberOfOperations) {
        this.set = set;
        this.numberOfReaders = numberOfReaders;
        this.numberOfWriters = numberOfWriters;
        this.numberOfOperations = numberOfOperations;
    }

    public void test() throws InterruptedException {
        ArrayList<Thread> threads = new ArrayList<>();

        for(int i = 0; i < numberOfReaders; i++) {
            threads.add(new Thread(new Reader(set, numberOfOperations)));
        }

        for(int i = 0; i < numberOfWriters; i++) {
            threads.add(new Thread(new Writer(set, numberOfOperations)));
        }

        for(Thread thread : threads) {
            thread.start();
        }

        for(Thread thread : threads) {
            thread.join();
        }
    }
}
