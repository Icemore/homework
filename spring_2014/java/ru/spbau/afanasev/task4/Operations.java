package ru.spbau.afanasev.task4;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * The class contains basic second-order functions such as fold, map, etc. as static methods.
 *
 * @author Anton Afanasev
 * @version 1.0 28 Mar 2014
 */
public class Operations {
    /**
     * Folds a collection using a given function.
     * Using infix notation the result will be
     * res = (x1 `f` (x2 `f` ... (xn `f` ini))...)
     *
     * @param f   the function that will be used for fold
     * @param ini the initial value
     * @param c   the collection
     * @param <R> the type of the result
     * @param <T> the type of the elements in the collection
     * @return the result of folding
     */
    public static <R, T> R foldr(Function2<R, T, R> f, R ini, Collection<T> c) {
        @SuppressWarnings("unchecked")
        T[] array = (T[]) c.toArray();

        R result = ini;
        for (int i = array.length - 1; i >= 0; i--) {
            result = f.apply(array[i], result);
        }

        return result;
    }

    /**
     * Takes first n element of a collection.
     *
     * @param n   the amount of elements to be taken
     * @param c   the collection
     * @param <T> the type of the elements in the collection
     * @return first n elements of the collection or all of them if the size of the collection less than n
     */
    public static <T> List<T> take(int n, Collection<T> c) {
        List<T> res = new ArrayList<>(n);

        int counter = 0;
        for (T item : c) {
            res.add(item);
            counter++;

            if (counter >= n) {
                break;
            }
        }

        return res;
    }

    /**
     * Applies a function to each element of a collection creating a new list of elements.
     *
     * @param f   the function to be applied
     * @param c   the collection
     * @param <R> the type of the resulting elements
     * @param <T> the type of the element of the given collection
     * @return the new list containing the result of applying f to each element of c
     */
    public static <R, T> List<R> map(Function<R, ? super T> f, Collection<T> c) {
        List<R> res = new ArrayList<>();

        for (T item : c) {
            res.add(f.apply(item));
        }

        return res;
    }

    /**
     * Filters a collection by peaking only elements on which given predicate is true.
     *
     * @param p   the predicate
     * @param c   the collection
     * @param <T> the type of the elements in the collection
     * @return the new list containing only elements of c on which p was true
     */
    public static <T> List<T> filter(Predicate<? super T> p, Collection<T> c) {
        List<T> res = new ArrayList<>();

        for (T item : c) {
            if (p.apply(item)) {
                res.add(item);
            }
        }

        return res;
    }

    /**
     * Performs currying of a two argument function to an one argument function
     * by passing given value as a first argument.
     *
     * @param func the function to be curried
     * @param arg1 the value to be placed instead the first argument
     * @param <R>  the type of the return value of the resulting function
     * @param <T1> the type of the arg1
     * @param <T2> the type of the argument of the resulting function
     * @return the function g(x) = func(arg1, x)
     */
    public static <R, T1, T2> Function<R, T2> bind1(final Function2<R, ? super T1, T2> func, final T1 arg1) {
        return new Function<R, T2>() {
            @Override
            public R apply(T2 arg2) {
                return func.apply(arg1, arg2);
            }
        };
    }

    /**
     * Performs curring of a two argument function to an one argument function
     * by passing given value as a second argument.
     *
     * @param func the function to be curried
     * @param arg2 the value to be placed instead the second argument
     * @param <R>  the type of the return value of the resulting function
     * @param <T1> the type of the arg2
     * @param <T2> the type of the argument of the resulting function
     * @return the function g(x) = func(x, arg2)
     */
    public static <R, T1, T2> Function<R, T1> bind2(final Function2<R, T1, ? super T2> func, final T2 arg2) {
        return new Function<R, T1>() {
            @Override
            public R apply(T1 arg1) {
                return func.apply(arg1, arg2);
            }
        };
    }
}
