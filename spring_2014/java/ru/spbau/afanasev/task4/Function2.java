package ru.spbau.afanasev.task4;

/**
 * The class represents a function with two arguments.
 *
 * @param <R>  the type of the function's return value
 * @param <T1> the type of the function's first argument
 * @param <T2> the type of the function's second argument
 * @author Anton Afanasev
 * @version 1.0 28 Mar 2014
 */
public abstract class Function2<R, T1, T2> {
    /**
     * Evaluate the function.
     *
     * @param first  first argument
     * @param second second argument
     * @return the result of evaluation
     */
    public abstract R apply(T1 first, T2 second);

    /**
     * Creates a new function that is the composition
     * of this and given function.
     * <p/>
     * res(x, y) = f(this(x, y))
     *
     * @param next the function that would be evaluated after this
     * @param <R1> the type of the return value of the resulting function
     * @return the new function that is the composition f(this)
     */
    public <R1> Function2<R1, T1, T2> then(final Function<R1, ? super R> next) {
        final Function2<R, T1, T2> inside = this;

        return new Function2<R1, T1, T2>() {
            @Override
            public R1 apply(T1 first, T2 second) {
                return next.apply(inside.apply(first, second));
            }
        };
    }
}
