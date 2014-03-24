package ru.spbau.afanasev.task3;

import java.io.*;
import java.util.zip.ZipInputStream;

/**
 * The class decompresses files created by Archiver.
 *
 * @author Anton Afanasev
 * @version 1.0 19 Mar 2014
 */
public class Dearchiver {
    /**
     * Dearchives files compressed by Archiver.
     * Puts every file from archive on relative path with which it was compressed.
     * If any folder on that relative path does not exists, method creates it.
     *
     * @param inputFileName path to archive to be decompressed
     * @throws IOException if an IO error occurs
     */
    public void dearchive(String inputFileName) throws IOException {
        try (ZipInputStream zis = new ZipInputStream(new FileInputStream(inputFileName));
             DataInputStream dis = new DataInputStream(zis)) {
            if (zis.getNextEntry() == null) {
                throw new EOFException("missing zip entry");
            }

            while (processFile(dis)) ;
        } catch (SecurityException e) {
            throw new IOException(e.getMessage());
        }
    }

    private void createFolders(String path) throws IOException {
        File dir = new File(path).getParentFile();

        if (dir.exists()) return;

        if (!dir.mkdirs()) {
            throw new IOException("can't create folders in path " + path);
        }
    }

    private boolean processFile(DataInputStream dis) throws IOException {
        int pathLength;

        try {
            pathLength = dis.readInt();
        } catch (EOFException ex) {
            return false;
        }

        byte[] pathBytes = new byte[pathLength];
        dis.readFully(pathBytes);
        String path = new String(pathBytes);

        createFolders(path);

        long fileLength = dis.readLong();
        try (FileOutputStream fos = new FileOutputStream(path)) {
            byte[] buffer = new byte[1024];
            long written = 0;

            while (written < fileLength) {
                int read = dis.read(buffer, 0, (int) Math.min(buffer.length, fileLength - written));

                if (read < 0) {
                    throw new EOFException("unexpected end of file");
                }

                fos.write(buffer, 0, read);
                written += read;
            }
        }

        System.out.println("File written " + path);

        return true;
    }
}