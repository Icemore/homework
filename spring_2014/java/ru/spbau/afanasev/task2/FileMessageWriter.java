package ru.spbau.afanasev.task2;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

/**
 * Writes messages to the file in the following format:
 * <p/>
 * number of lines in the message
 * lines of the message
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class FileMessageWriter implements MessageWriter {
    private BufferedWriter writer;

    /**
     * Constructs FileMessageWriter to write in the specified file.
     *
     * @param fileName the name of the output file
     * @throws IOException if an IO error occurs
     */
    public FileMessageWriter(String fileName) throws IOException {
        writer = new BufferedWriter(new FileWriter(fileName));
    }

    @Override
    public void writeMessage(Message message) throws IOException {
        List<String> lines = message.getLines();

        writer.write(String.valueOf(lines.size()));
        writer.newLine();

        for (String line : lines) {
            writer.write(line);
            writer.newLine();
        }
    }

    @Override
    public void close() throws IOException {
        writer.close();
    }
}
