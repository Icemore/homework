package ru.spbau.afanasev.task2;

/**
 * Thrown when attempt to read message failed due to the bad message formatting.
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class IllegalMessageFormatException extends Exception {
    /**
     * Constructs a IllegalMessageFormatException with
     * the specified detail message
     *
     * @param message the detail message
     */
    public IllegalMessageFormatException(String message) {
        super(message);
    }
}
