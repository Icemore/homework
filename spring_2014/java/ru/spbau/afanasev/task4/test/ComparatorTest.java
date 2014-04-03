package ru.spbau.afanasev.task4.test;


import org.junit.Test;
import ru.spbau.afanasev.task4.Comparator;
import ru.spbau.afanasev.task4.Function;

import static org.junit.Assert.assertTrue;

public class ComparatorTest {
    @Test
    public void testComparator() throws Exception {
        Function<Integer, A> f = new Function<Integer, A>() {
            @Override
            public Integer apply(A arg) {
                return arg.a;
            }
        };

        Comparator<A, Integer> cmp = new Comparator<>(f);

        A one = new A(1, 234);
        A two = new A(2, 254);
        A t = new A(2, 212);

        assertTrue(cmp.apply(two, t) == 0);
        assertTrue(cmp.apply(one, two) < 0);
        assertTrue(cmp.apply(t, one) > 0);
    }

    class A {
        public int a, b;

        A(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }
}
