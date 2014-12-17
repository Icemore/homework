package ru.spbau.afanasev.LockFreeList;

import java.util.Random;

public class Writer extends Worker {
    protected Writer(Set<Integer> set, int numberOfOperations) {
        super(set, numberOfOperations);
    }

    @Override
    public void run() {
        Random rand = new Random();

        for(int i = 0; i < numberOfOperations; i++) {
            int key = rand.nextInt(MAX_KEY);

            if(rand.nextInt() % 2 == 0) {
                set.add(key);
            }
            else {
                set.remove(key);
            }
        }
    }
}
