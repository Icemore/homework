package ru.spbau.afanasev;

import java.util.concurrent.Callable;

/**
 *  Worker represents one working thread.
 *  Waits for task to be assigned and executes it. Also controls cold threads' time outs: if task is not coming for
 *  a specified time period initiates time out termination process.
 */
public class Worker implements Runnable {
    private ThreadPool threadPool;
    private int workerId;
    private long waitingTime;
    private volatile boolean stopped;

    private Task<?> task;
    private Thread workingThread;
    private final Object taskLock = new Object();

    public Worker(int workerId, ThreadPool threadPool, long waitingTime) {
        this.workerId = workerId;
        this.threadPool = threadPool;
        this.waitingTime = waitingTime;
        this.stopped = false;

        workingThread = new Thread(this);
        workingThread.start();
    }

    public int getWorkerId() {
        return workerId;
    }

    public void run() {
        while (!stopped) {
            try {
                if (waitForTask()) {
                    processTask();
                } else {
                    notifyTimeout();
                }
            } catch (InterruptedException ignored) {
            }
        }

        threadPool.reportWorkerStopped(this);
    }

    private void notifyTimeout() {
        threadPool.reportWorkerTimeout(this);
    }

    private void processTask() {
        task.run();

        synchronized (taskLock) {
            task = null;
            Thread.interrupted(); // clean possible interruption flag
        }
        threadPool.reportWorkerDone(this);
    }

    private boolean waitForTask() throws InterruptedException {
        synchronized (taskLock) {
            long startedWaiting = System.currentTimeMillis();
            long alreadyWaited = 0;

            while (task == null &&
                    (waitingTime == 0 || alreadyWaited < waitingTime)) {
                taskLock.wait(waitingTime == 0 ? 0 : waitingTime - alreadyWaited);
                alreadyWaited = System.currentTimeMillis() - startedWaiting;
            }
            return task != null;
        }
    }

    public <T> Task<T> acceptTask(Callable<T> callable) {
        Task<T> task = new Task<>(callable, workingThread);

        synchronized (taskLock) {
            this.task = task;
            taskLock.notify();
        }

        return task;
    }

    public void stopWorker() {
        synchronized (taskLock) {
            stopped = true;
            workingThread.interrupt();
        }
    }
}
