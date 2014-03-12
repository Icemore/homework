package ru.spbau.afanasev.task2;

import java.io.IOException;

/**
 * Writes messages to an MessageWriter merging to consecutive messages
 * to the one. If last message does not have a pair it is written as is.
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class CompressingMessageWriter implements MessageWriter {
    private MessageWriter writer;
    private Message buf;

    /**
     * Constructs CompressingMessageWriter given a writer
     *
     * @param writer a MessageWriter
     */
    public CompressingMessageWriter(MessageWriter writer) {
        this.writer = writer;
        buf = null;
    }

    @Override
    public void writeMessage(Message message) throws IOException {
        if (buf == null) {
            buf = new Message();
            buf.append(message);
        } else {
            buf.append(message);
            writer.writeMessage(buf);
            buf = null;
        }
    }

    @Override
    public void close() throws IOException {
        try {
            if (buf != null) {
                writer.writeMessage(buf);
            }
        } finally {
            writer.close();
        }
    }
}
