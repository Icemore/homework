package ru.spbau.afanasev.task4;

/**
 * The class represents a predicate - a function that returns bool and provides methods for combining predicates
 * in logical expressions.
 * Also provides several static methods that gives some basic predicates.
 *
 * @param <T> the type of the argument of predicate
 * @author Anton Afanasev
 * @version 1.0 28 Mar 2014
 */
public abstract class Predicate<T> extends Function<Boolean, T> {
    /**
     * Creates a predicate that is always true.
     *
     * @return the predicate p(x) = true
     */
    static public Predicate<Object> alwaysTrue() {
        return new Predicate<Object>() {
            @Override
            public Boolean apply(Object arg) {
                return true;
            }
        };
    }

    /**
     * Creates a predicate that is always false.
     *
     * @return the predicate p(x) = false
     */
    static public Predicate<Object> alwaysFalse() {
        return new Predicate<Object>() {
            @Override
            public Boolean apply(Object arg) {
                return false;
            }
        };
    }

    /**
     * Creates a predicate that is true only when argument is not null.
     *
     * @return the new predicate p(x) =  (x != null)
     */
    static public Predicate<Object> notNull() {
        return new Predicate<Object>() {
            @Override
            public Boolean apply(Object arg) {
                return arg != null;
            }
        };
    }

    /**
     * Creates a predicate that is true when argument equals a given value.
     *
     * @param a   the value to which predicate will be comparing
     * @param <S> the type of the argument of the resulting predicate
     * @return the new predicate p(x) = (x == a)
     */
    static public <S> Predicate<S> equals(final Comparable<? super S> a) {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S arg) {
                return a.compareTo(arg) == 0;
            }
        };
    }

    /**
     * Creates a predicate that is true when argument less than a given value.
     *
     * @param a   the value to which predicate will be comparing
     * @param <S> the type of the argument of the resulting predicate
     * @return the new predicate p(x) == (x < a)
     */
    static public <S> Predicate<S> less(final Comparable<? super S> a) {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S arg) {
                return a.compareTo(arg) > 0;
            }
        };
    }

    /**
     * Creates the new predicate that is the negation of this.
     *
     * @return the new predicate that is the negation of this
     */
    public Predicate<T> not() {
        final Predicate<T> p = this;

        return new Predicate<T>() {
            @Override
            public Boolean apply(T arg) {
                return !p.apply(arg);
            }
        };
    }

    /**
     * Creates a new predicate that is true only when this and given predicates are both true.
     *
     * @param other the second predicate in and operation
     * @return the new predicate p(x) = this(x) && other(x)
     */
    public Predicate<T> and(final Predicate<? super T> other) {
        final Predicate<T> me = this;

        return new Predicate<T>() {
            @Override
            public Boolean apply(T arg) {
                return me.apply(arg) && other.apply(arg);
            }
        };
    }

    /**
     * Create a new predicate that is true when either this or given predicate is true.
     *
     * @param other the second predicate in or operation
     * @return the new predicate p(x) = this(x) || other(x)
     */
    public Predicate<T> or(final Predicate<? super T> other) {
        final Predicate<T> me = this;

        return new Predicate<T>() {
            @Override
            public Boolean apply(T arg) {
                return me.apply(arg) || other.apply(arg);
            }
        };
    }
}
