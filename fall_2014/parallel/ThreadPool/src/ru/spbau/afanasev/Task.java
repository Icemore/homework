package ru.spbau.afanasev;

import java.util.concurrent.Callable;

/**
 * The task that is executed by worker.
 * Allows to check status of computation (done, working, etc.), interrupt it or get the results.
 */
public class Task<T> {
    private volatile Status status;
    private T result;
    private final Object resultLock = new Object();
    private Callable<T> routine;
    private Thread workingThread;

    public enum Status {
        WORKING, DONE, INTERRUPTED, FAILED
    }

    public static class ComputationFailedException extends Exception {
        public ComputationFailedException(String message) {
            super(message);
        }

        public ComputationFailedException() {
        }
    }

    public Task(Callable<T> routine, Thread workingThread) {
        this.routine = routine;
        this.workingThread = workingThread;
        status = Status.WORKING;
    }

    public Status getStatus() {
        synchronized (resultLock) {
            return status;
        }
    }

    public void interrupt() {
        synchronized (resultLock) {
            if (status == Status.WORKING) {
                workingThread.interrupt();
            }
        }
    }

    public void run() {
        Status curStatus;
        T curRes = null;

        try {
            curRes = routine.call();
            curStatus = Status.DONE;
        } catch (InterruptedException ex) {
            curStatus = Status.INTERRUPTED;
            Thread.interrupted();
        } catch (Exception ex) {
            curStatus = Status.FAILED;
        }

        synchronized (resultLock) {
            status = curStatus;
            result = curRes;
            resultLock.notifyAll();
        }
    }

    public T getResult() throws InterruptedException, ComputationFailedException {
        synchronized (resultLock) {
            while (status == Status.WORKING) {
                resultLock.wait();
            }

            if (status != Status.DONE) {
                throw new ComputationFailedException();
            } else {
                return result;
            }
        }
    }
}
