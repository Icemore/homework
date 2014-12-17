package ru.spbau.afanasev.LockFreeList;


public abstract class Worker implements Runnable {
    protected final static int MAX_KEY = 100000;
    Set<Integer> set;
    int numberOfOperations;

    protected Worker(Set<Integer> set, int numberOfOperations) {
        this.set = set;
        this.numberOfOperations = numberOfOperations;
    }
}
