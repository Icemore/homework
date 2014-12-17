package ru.spbau.afanasev.LockFreeList;

public class Main {
    private static Set<Integer> parseSet(String str) {
        if(str.equals("block")) {
            return new BlockingList<>();
        }

        if(str.equals("lock-free")) {
            return new LockFreeList<>();
        }

        throw new IllegalArgumentException("bad list type name");
    }

    private static void printUsage() {
        System.out.println("arguments: readers writers operations type (either 'block' or 'lock-free')");
    }

    public static void main(String[] args) {
        if(args.length < 4) {
            System.out.println("Not enough arguments");
            printUsage();
            return;
        }

        try {
            int numberOfReaders = Integer.parseInt(args[0]);
            int numberOfWriters = Integer.parseInt(args[1]);
            int numberOfOperations = Integer.parseInt(args[2]);
            Set<Integer> set = parseSet(args[3]);

            Tester tester = new Tester(set, numberOfReaders, numberOfWriters, numberOfOperations);
            tester.test();
        }
        catch(IllegalArgumentException ex) {
            System.out.println("Wrong arguments");
            printUsage();
        } catch (InterruptedException ignored) {
        }
    }
}
