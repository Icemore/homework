package ru.spbau.afanasev.task4;

/**
 * The class represents a function with one argument.
 *
 * @param <R> the type of the function's return value
 * @param <T> the type of the function's argument
 * @author Anton Afanasev
 * @version 1.0 26 Mar 2014
 */
public abstract class Function<R, T> {
    /**
     * Evaluate the function.
     *
     * @param arg function argument
     * @return the result of evaluation
     */
    public abstract R apply(T arg);

    /**
     * Creates new function that is the composition of this
     * and given function.
     * <p/>
     * res(x) = f(this(x))
     *
     * @param next the function that will be applied after this
     * @param <R1> the type of the return value of the resulting function
     * @return the new function that is the composition f(this)
     */
    public <R1> Function<R1, T> then(final Function<R1, ? super R> next) {
        final Function<R, T> me = this;

        return new Function<R1, T>() {
            @Override
            public R1 apply(T arg) {
                return next.apply(me.apply(arg));
            }
        };
    }
}
