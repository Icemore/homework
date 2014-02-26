package ru.spbau.afanasev.task1;

/**
 * The class, being run from command line, takes as arguments integer numbers
 * separated by whitespaces and prints their sum.
 *
 * @author Anton Afanasev
 * @version 1.0 25 Feb 2014
 */
public class Sum {
    /**
     * Entry point.
     * Takes program arguments, which has to be integers
     * separated by whitespaces and prints their sum.
     *
     * @param args whitespace separated integers
     */
    public static void main(String[] args) {
        int sum = 0;

        for (String str : args) {
            for (String token : str.split("\\s+")) {
                sum += strToInt(token);
            }
        }

        System.out.println(sum);
    }

    /**
     * Converts the integer number in string representation to int.
     * Expects string to be a number without any characters other than digits.
     *
     * @param str a string representing the number
     * @return the number from string as int
     */
    private static int strToInt(String str) {
        int result = 0;

        for (int i = 0; i < str.length(); i++) {
            result = result * 10 + str.charAt(i) - '0';
        }

        return result;
    }
}
