package ru.spbau.afanasev.task4.test;

import org.junit.Test;
import ru.spbau.afanasev.task4.Predicate;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PredicateTest {
    private Predicate<Integer> divisibleBy3;
    private Predicate<Integer> less14;

    public PredicateTest() {
        divisibleBy3 = new Predicate<Integer>() {
            @Override
            public Boolean apply(Integer arg) {
                return arg % 3 == 0;
            }
        };

        less14 = new Predicate<Integer>() {
            @Override
            public Boolean apply(Integer arg) {
                return arg < 14;
            }
        };
    }

    @Test
    public void testNot() throws Exception {
        Predicate<Integer> p = divisibleBy3.not();

        assertTrue(p.apply(5));
        assertFalse(p.apply(33));
    }

    @Test
    public void testAnd() throws Exception {
        Predicate<Integer> p = divisibleBy3.and(less14);

        assertTrue(p.apply(9));
        assertFalse(p.apply(5));
        assertFalse(p.apply(15));
        assertFalse(p.apply(17));
    }

    @Test
    public void testOr() throws Exception {
        Predicate<Integer> p = divisibleBy3.or(less14);

        assertTrue(p.apply(6));
        assertTrue(p.apply(7));
        assertTrue(p.apply(15));
        assertFalse(p.apply(17));
    }

    @Test
    public void testAlwaysTrue() throws Exception {
        Predicate<Object> t = Predicate.alwaysTrue();

        assertTrue(t.apply(5));
    }

    @Test
    public void testAlwaysFalse() throws Exception {
        Predicate<Object> t = Predicate.alwaysFalse();

        assertFalse(t.apply(134));
    }

    @Test
    public void testNotNull() throws Exception {
        Integer i = 45;
        Object r = "asd";

        Predicate<Object> notNullPred = Predicate.notNull();

        assertTrue(notNullPred.apply(i));
        assertTrue(notNullPred.apply(r));
        assertFalse(notNullPred.apply(null));
    }

    @Test
    public void testEquals() throws Exception {
        Predicate<Integer> equalsOne = Predicate.equals(1);

        assertTrue(equalsOne.apply(1));
        assertFalse(equalsOne.apply(3));
    }

    @Test
    public void testLess() throws Exception {
        Predicate<Integer> less50 = Predicate.less(50);

        assertTrue(less50.apply(40));
        assertFalse(less50.apply(50));
        assertFalse(less50.apply(100));
    }
}
