package ru.spbau.afanasev.task2;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * Reads messages from file.
 * Messages should be in the following format:
 * <p/>
 * number of lines in message 1
 * lines of message 1
 * number of lines in message 2
 * lines of message 2
 * and so on
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class FileMessageReader implements AutoCloseable {
    private BufferedReader reader;

    /**
     * Creates an instance of FileMessageReader, given the name of the file
     * to read from.
     *
     * @param fileName the name of the file to read
     * @throws FileNotFoundException if the file does not exists
     *                               or cannot be opened
     */
    public FileMessageReader(String fileName) throws FileNotFoundException {
        this.reader = new BufferedReader(new FileReader(fileName));
    }

    /**
     * Reads one message from file.
     *
     * @return next message from file or null if the end of the file has been reached
     * @throws IllegalMessageFormatException if wrong message formatting encountered
     * @throws IOException                   if an IO error occurs
     */
    public Message readMessage() throws IllegalMessageFormatException, IOException {
        String linesCntString = reader.readLine();

        if (linesCntString == null)
            return null;

        int linesCnt;
        Message message = new Message();

        try {
            linesCnt = Integer.parseInt(linesCntString.trim());
        } catch (NumberFormatException e) {
            throw new IllegalMessageFormatException("number of lines in message expected; got:" + linesCntString);
        }

        for (int i = 0; i < linesCnt; i++) {
            String line = reader.readLine();

            if (line == null) {
                throw new IllegalMessageFormatException("unexpected EOF");
            }

            message.append(line);
        }

        return message;
    }

    @Override
    public void close() throws IOException {
        reader.close();
    }
}
