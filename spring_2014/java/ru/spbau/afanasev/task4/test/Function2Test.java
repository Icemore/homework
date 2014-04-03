package ru.spbau.afanasev.task4.test;

import org.junit.Assert;
import org.junit.Test;
import ru.spbau.afanasev.task4.Function;
import ru.spbau.afanasev.task4.Function2;

public class Function2Test {
    @Test
    public void testThen() throws Exception {
        Function2<Integer, Double, Double> f = new Function2<Integer, Double, Double>() {
            @Override
            public Integer apply(Double first, Double second) {
                return Double.valueOf((first + second) / 2).intValue();
            }
        };

        Function<String, Integer> g = new Function<String, Integer>() {
            @Override
            public String apply(Integer arg) {
                return Integer.toString(arg * arg);
            }
        };

        Function2<String, Double, Double> c = f.then(g);

        Assert.assertEquals(c.apply(2.0, 5.2), "9");
    }
}
