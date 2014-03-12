package ru.spbau.afanasev.task2;

import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Entry point class.
 * Reads messages from the given file, merges pairs of consecutive messages
 * and prints them to another file or to the standard output if the output file
 * is not specified.
 *
 * @author Anton Afanasev
 * @version 1.0 11 Mar 2014
 */
public class Main {
    /**
     * Entry point.
     * Reads messages from the given file, merges pairs of consecutive messages
     * and prints them to another file or to the standard output if the output file
     * is not specified.
     *
     * @param args inputFileName [outputFileName]
     *             first argument - name of the input file
     *             second argument (optional) - name of the output file
     *                  if not specified messages are printed to the standard output
     */
    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Too few arguments");
            return;
        }

        String inputFileName = args[0];
        String outputFileName = args.length > 1 ? args[1] : null;

        try (FileMessageReader reader = new FileMessageReader(inputFileName);
             MessageWriter writer = outputFileName != null ?
                     new CompressingMessageWriter(new FileMessageWriter(outputFileName)) :
                     new CompressingMessageWriter(new ConsoleMessageWriter())) {

            Message message;

            while ((message = reader.readMessage()) != null) {
                writer.writeMessage(message);
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + inputFileName);
        } catch (IllegalMessageFormatException e) {
            System.err.println("Wrong message format: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("IOException happened. Message: " + e.getMessage());
        }
    }
}
