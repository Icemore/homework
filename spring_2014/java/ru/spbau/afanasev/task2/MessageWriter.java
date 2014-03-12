package ru.spbau.afanasev.task2;

import java.io.Closeable;
import java.io.IOException;

/**
 * A writer that can write messages.
 * Writer is also closable.
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public interface MessageWriter extends Closeable {
    /**
     * Writes one message to some output stream.
     *
     * @param message the massage to be written
     * @throws IOException is an IO error occurs
     */
    void writeMessage(Message message) throws IOException;
}