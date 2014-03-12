package ru.spbau.afanasev.task2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A message which consists of a number of lines.
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class Message {
    private List<String> lines = new ArrayList<>();

    /**
     * Appends line to the message
     *
     * @param line a line to be appended
     */
    public void append(String line) {
        lines.add(line);
    }

    /**
     * Appends lines of the other message to this message
     *
     * @param msg message which lines to be appended to this message
     */
    public void append(Message msg) {
        lines.addAll(msg.getLines());
    }

    /**
     * @return unmodifiable list of lines of the message
     */
    public List<String> getLines() {
        return Collections.unmodifiableList(lines);
    }

    /**
     * Deletes all lines
     */
    public void clear() {
        lines.clear();
    }
}
