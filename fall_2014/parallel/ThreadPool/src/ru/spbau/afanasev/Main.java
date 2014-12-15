package ru.spbau.afanasev;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.Callable;

public class Main {
    private static volatile boolean stopped = false;

    public static class TimedTask implements Callable<Integer> {
        private long time;
        private int taskId;

        public TimedTask(int taskId, long time) {
            this.time = time * 1000;
            this.taskId = taskId;
        }

        @Override
        public Integer call() throws Exception {
            System.out.println("task " + taskId + " started");
            Thread.sleep(time);
            System.out.println("task " + taskId + " stopped");
            return taskId;
        }
    }

    public static void run(int numberOfHotThreads, int coldThreadTimeout) {
        ThreadPool pool = new ThreadPool(numberOfHotThreads, coldThreadTimeout * 1000);
        int tasksCount = 0;
        Map<Integer, Task> taskMap = new HashMap<>();

        Scanner scanner = new Scanner(System.in);

        while (!stopped) {
            if (!scanner.hasNext()) {
                break;
            }

            String cmd = scanner.next();
            switch (cmd) {
                case "add":
                    long duration = scanner.nextLong();

                    System.out.println("task #" + tasksCount + " put to pool");
                    Task task = pool.addTask(new TimedTask(tasksCount, duration));
                    taskMap.put(tasksCount, task);
                    tasksCount++;
                    break;

                case "int":
                    int taskId = scanner.nextInt();
                    taskMap.get(taskId).interrupt();
                    break;

                case "exit":
                    stopped = true;
                    break;

                default:
                    System.out.println("Wrong operation: " + cmd);
            }
        }

        System.out.println("shutting down thread pool");
        pool.shutdown();
    }

    private static void setupHook() {
        final Thread mainThread = Thread.currentThread();
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                stopped = true;
                try {
                    mainThread.join();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    public static void main(String[] args) {
        setupHook();

        if (args.length < 2) {
            System.err.println("Not enough arguments");
            return;
        }


        int numberOfHotThreads = Integer.parseInt(args[0]);
        int coldThreadTimeout = Integer.parseInt(args[1]);
        run(numberOfHotThreads, coldThreadTimeout);
    }
}
