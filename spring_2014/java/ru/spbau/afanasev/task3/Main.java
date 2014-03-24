package ru.spbau.afanasev.task3;

import java.io.EOFException;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Compresses and decompresses files on disk using zip algorithm.
 *
 * @author Anton Afanasev
 * @version 1.0 19 Mar 2014
 */
public class Main {
    /**
     * Entry point.
     * First argument should be either "compress" or "decompress".
     * <p/>
     * If first argument is "compress" second argument is treated as output file name and the rest of arguments
     * is files and folders to be compressed. In case of directories all files contained inside this directory
     * and all subdirectory would be added.
     * <p/>
     * If first argument is "decompress" second argument is treated as path to the archive to be decompressed.
     * All containing files would be decompresses and put on their relative paths with which they were compressed.
     * If some folders does not exist they would be created.
     *
     * @param args compress outputFileName [files and directories to be compressed]
     *             or
     *             decompress inputFileName
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Too few arguments");
            return;
        }

        boolean isOk = false;
        switch (args[0]) {
            case "compress":
                try {
                    compress(args[1], Arrays.asList(args).subList(2, args.length));
                    isOk = true;
                } catch (IOException e) {
                    System.err.println("IO exception happened: \n" + e.getMessage());
                }

                if (isOk) {
                    System.out.println("Compression ended successfully");
                } else {
                    System.out.println("Compression failed");
                }
                break;
            case "decompress":
                try {
                    decompress(args[1]);
                    isOk = true;
                } catch (EOFException e) {
                    System.err.println("Bad archive: " + e.getMessage());
                } catch (IOException e) {
                    System.err.println("IO exception happened: \n" + e.getMessage());
                }

                if (isOk) {
                    System.out.println("Decompression ended successfully");
                } else {
                    System.out.println("Decompression failed");
                }
                break;
            default:
                System.err.println("Wrong argument. Expected compress or decompress");
                break;
        }
    }

    private static void compress(String outputFileName, List<String> filesToCompress) throws IOException {
        try (Archiver archiver = new Archiver(outputFileName)) {
            for (String filePath : filesToCompress) {
                try {
                    archiver.putAllFiles(filePath);
                } catch (IOException ex) {
                    throw new IOException("exception during compressing " + filePath + ": \n" + ex.getMessage());
                }
            }
        }
    }

    private static void decompress(String inputFileName) throws IOException {
        Dearchiver dearchiver = new Dearchiver();

        dearchiver.dearchive(inputFileName);
    }
}
