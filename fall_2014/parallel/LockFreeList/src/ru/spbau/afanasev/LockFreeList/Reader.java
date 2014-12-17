package ru.spbau.afanasev.LockFreeList;

import java.util.Random;

public class Reader extends Worker {
    protected Reader(Set<Integer> set, int numberOfOperations) {
        super(set, numberOfOperations);
    }

    @Override
    public void run() {
        Random rand = new Random();

        for(int i = 0; i < numberOfOperations; i++) {
            int key = rand.nextInt(MAX_KEY);
            set.contains(key);
        }
    }
}
