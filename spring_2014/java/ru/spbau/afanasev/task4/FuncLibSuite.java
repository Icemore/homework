package ru.spbau.afanasev.task4;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import ru.spbau.afanasev.task4.test.*;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        ComparatorTest.class,
        FunctionTest.class,
        Function2Test.class,
        OperationsTest.class,
        PredicateTest.class
})
public class FuncLibSuite {
}
