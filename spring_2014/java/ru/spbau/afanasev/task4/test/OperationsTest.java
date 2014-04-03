package ru.spbau.afanasev.task4.test;

import org.junit.Test;
import ru.spbau.afanasev.task4.Function;
import ru.spbau.afanasev.task4.Function2;
import ru.spbau.afanasev.task4.Operations;
import ru.spbau.afanasev.task4.Predicate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class OperationsTest {
    List<Integer> intList;

    public OperationsTest() {
        intList = Arrays.asList(3, 5, 6, 24, 2, 12);
    }

    @Test
    public void testFoldr() throws Exception {
        Function2<String, Integer, String> f = new Function2<String, Integer, String>() {
            @Override
            public String apply(Integer first, String second) {
                return first.toString() + second;
            }
        };

        String res = Operations.foldr(f, "", intList);
        assertEquals("35624212", res);
    }

    @Test
    public void testTake() throws Exception {
        List<Integer> res = Operations.take(3, intList);

        assertEquals(intList.subList(0, 3), res);
    }

    @Test
    public void testMap() throws Exception {
        Function<Integer, Integer> f = new Function<Integer, Integer>() {
            @Override
            public Integer apply(Integer arg) {
                return arg * 2;
            }
        };

        List<Integer> res = Operations.map(f, intList);
        List<Integer> expected = new ArrayList<>();

        for (int i : intList) {
            expected.add(i * 2);
        }

        assertEquals(expected, res);
    }

    @Test
    public void testFilter() throws Exception {
        Predicate<Integer> p = new Predicate<Integer>() {
            @Override
            public Boolean apply(Integer arg) {
                return arg < 5;
            }
        };

        List<Integer> res = Operations.filter(p, intList);
        List<Integer> expected = new ArrayList<>();

        for (int i : intList) {
            if (i < 5) {
                expected.add(i);
            }
        }

        assertEquals(expected, res);
    }

    @Test
    public void testBind1() throws Exception {
        Function2<Integer, Integer, Double> mul = new Function2<Integer, Integer, Double>() {
            @Override
            public Integer apply(Integer first, Double second) {
                return first * second.intValue();
            }
        };

        Function<Integer, Double> mul5 = Operations.bind1(mul, 5);

        assertEquals(mul5.apply(0.5).intValue(), 0);
        assertEquals(mul5.apply(4.2).intValue(), 20);
    }

    @Test
    public void testBind2() throws Exception {
        Function2<Integer, Integer, Double> mul = new Function2<Integer, Integer, Double>() {
            @Override
            public Integer apply(Integer first, Double second) {
                return first * second.intValue();
            }
        };

        Function<Integer, Integer> mul2 = Operations.bind2(mul, 2.7);

        assertEquals(mul2.apply(3).intValue(), 6);
    }
}
