package ru.spbau.afanasev.task3;

import java.io.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Archives given files to one, compressed with zip algorithm.
 * Creates zip file with one entry and writes to it data in the following format
 *
 * #1   4 bytes (int)   length of the relative path to the first file
 * #2   #1 bytes        relative path to the first file
 * #3   8 bytes (long)  size of the first file
 * #4   #3 bytes        content of the first file
 * and so on for the second and subsequent files
 *
 * @author Anton Afanasev
 * @version 1.0 19 Mar 2014
 */
public class Archiver implements Closeable {
    private DataOutputStream dos;

    /**
     * Creates an instance of Archiver
     *
     * @param outputFileName path to the resulting archive
     * @throws IOException if an IO error occurs
     */
    public Archiver(String outputFileName) throws IOException {
        try {
            ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(outputFileName));
            zos.putNextEntry(new ZipEntry("data"));

            dos = new DataOutputStream(zos);
        } catch (SecurityException e) {
            throw new IOException(e.getMessage());
        }
    }

    /**
     * If given a file adds it to the archive.
     * If given a directory walks it recursively and adds all found files.
     *
     * @param path file or directory with files to be added to the archive
     * @throws IOException if an IO error occurs
     */
    public void putAllFiles(String path) throws IOException {
        File file = new File(path);

        try {
            if (!file.exists()) {
                throw new FileNotFoundException("File not found: " + path);
            }

            putAllFiles(file);
        } catch (SecurityException e) {
            throw new IOException(e.getMessage());
        }
    }

    private void putAllFiles(File currentFile) throws IOException {
        if (currentFile.isFile()) {
            putNextFile(currentFile);
        } else {
            File[] fileList = currentFile.listFiles();

            if (fileList == null) {
                throw new IOException("can't list files in " + currentFile.getPath());
            }

            for (File file : fileList) {
                putAllFiles(file);
            }
        }
    }

    private void putNextFile(File file) throws IOException {
        String path = file.getPath();

        if (!file.canRead())
            throw new FileNotFoundException("can't read " + file.getPath());

        FileInputStream fis = new FileInputStream(file);
        byte[] buffer = new byte[1024];

        dos.writeInt(path.length());
        dos.write(path.getBytes());
        dos.writeLong(file.length());

        int length;
        while ((length = fis.read(buffer)) > 0) {
            dos.write(buffer, 0, length);
        }

        System.out.println("File added " + path);
    }

    @Override
    public void close() throws IOException {
        dos.close();
    }
}
