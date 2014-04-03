package ru.spbau.afanasev.task4;

/**
 * Comparator that compares objects of some class using a function to comparable objects.
 * Given a function f it will return f(a).compareTo(f(b)) for some a and b.
 *
 * @param <A> the type of an intermediate values by which comparison will be performed
 *            must be comparable
 * @param <T> the type of the values to be compared
 * @author Anton Afanasev
 * @version 1.0 28 Mar 2014
 */
public class Comparator<T, A extends Comparable<? super A>> extends Function2<Integer, T, T> {
    private Function<A, T> f;

    /**
     * Constructs a comparator with given function for comparison
     *
     * @param f the function from T to comparable objects
     */
    public Comparator(Function<A, T> f) {
        this.f = f;
    }

    @Override
    public Integer apply(T first, T second) {
        return f.apply(first).compareTo(f.apply(second));
    }
}
