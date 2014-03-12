package ru.spbau.afanasev.task2;

import java.util.List;

/**
 * Prints messages to the standard output stream in the following format:
 * <p/>
 * Message 1
 * 1.1. line 1
 * 1.2. line 2
 * Message 2
 * 2.1. line 1
 * 2.2. line 2
 * 2.3. line 3
 * and so on
 * <p/>
 * In order to do this maintains amount of printed messages.
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class ConsoleMessageWriter implements MessageWriter {
    private int messageCounter = 0;

    @Override
    public void writeMessage(Message message) {
        messageCounter++;

        System.out.println("Message " + Integer.toString(messageCounter));

        List<String> lines = message.getLines();
        for (int i = 0; i < lines.size(); i++) {
            System.out.printf("%d.%d. %s", messageCounter, i + 1, lines.get(i));
            System.out.println();
        }
    }

    @Override
    public void close() {
        // do nothing
    }
}
