package ru.spbau.afanasev.task4.test;

import org.junit.Assert;
import org.junit.Test;
import ru.spbau.afanasev.task4.Function;

public class FunctionTest {

    @Test
    public void testThen() throws Exception {
        Function<String, Long> f = new Function<String, Long>() {
            @Override
            public String apply(Long arg) {
                return Long.toString(arg * arg);
            }
        };

        Function<Long, Integer> g = new Function<Long, Integer>() {
            @Override
            public Long apply(Integer arg) {
                return arg.longValue() + 7;
            }
        };

        Function<String, Integer> c = g.then(f);

        Assert.assertEquals(c.apply(4), "121");
    }
}
