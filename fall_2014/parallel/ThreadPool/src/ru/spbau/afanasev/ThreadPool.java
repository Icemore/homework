package ru.spbau.afanasev;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;

public class ThreadPool {
    private int numberOfHotThreads;
    private long coldThreadTimeout;
    private int workerCount;

    private Map<Worker, Boolean> hasTask;
    private Map<Worker, Long> lastTimeHadTask;
    private final Object lock = new Object();

    public ThreadPool(int numberOfHotThreads, long coldThreadTimeout) {
        this.numberOfHotThreads = numberOfHotThreads;
        this.coldThreadTimeout = coldThreadTimeout;

        hasTask = new HashMap<>();
        lastTimeHadTask = new HashMap<>();

        init();
    }

    private void init() {
        for (int i = 0; i < numberOfHotThreads; i++) {
            hasTask.put(new Worker(i, this, 0), false);
        }
        workerCount = numberOfHotThreads;
    }

    public <T> Task<T> addTask(Callable<T> callable) {
        synchronized (lock) {
            for (Map.Entry<Worker, Boolean> entry : hasTask.entrySet()) {
                Worker worker = entry.getKey();
                boolean busy = entry.getValue();

                if (!busy) {
                    System.out.println("task put to worker #" + worker.getWorkerId());
                    hasTask.put(worker, true);
                    return worker.acceptTask(callable);
                }
            }

            // add cold thread
            System.out.println("all threads are busy, starting new one #" + workerCount);
            Worker newWorker = new Worker(workerCount++, this, coldThreadTimeout);
            hasTask.put(newWorker, true);
            return newWorker.acceptTask(callable);
        }
    }

    public void reportWorkerTimeout(Worker worker) {
        synchronized (lock) {
            if (!hasTask.get(worker) &&
                    lastTimeHadTask.get(worker) + coldThreadTimeout < System.currentTimeMillis()) {

                worker.stopWorker();

                // say that timed out thread is busy to prevent task assignment while it is stopping
                hasTask.put(worker, true);

                System.out.println("cold thread #" + worker.getWorkerId() + " timed out");
            }
        }
    }

    public void reportWorkerStopped(Worker worker) {
        synchronized (lock) {
            hasTask.remove(worker);
            lastTimeHadTask.remove(worker);

            System.out.println("thread #" + worker.getWorkerId() + " stopped");
            lock.notify();
        }
    }

    public void reportWorkerDone(Worker worker) {
        synchronized (lock) {
            hasTask.put(worker, false);
            lastTimeHadTask.put(worker, System.currentTimeMillis());
            System.out.println("thread #" + worker.getWorkerId() + " is free now");
        }
    }

    public void shutdown() {
        synchronized (lock) {
            for (Worker worker : hasTask.keySet()) {
                worker.stopWorker();
            }

            while (!hasTask.isEmpty()) {
                try {
                    lock.wait();
                } catch (InterruptedException e) {
                    Thread.interrupted();
                }
            }
        }
    }
}
