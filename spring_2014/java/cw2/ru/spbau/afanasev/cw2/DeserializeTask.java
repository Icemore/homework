package ru.spbau.afanasev.cw2;

import java.io.FileReader;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.Callable;

public class DeserializeTask<T> implements Callable<T> {
    String name;
    Class<T> tClass;

    public DeserializeTask(Class<T> tClass, String name) {
        this.tClass = tClass;
        this.name = name;
    }

    @Override
    public T call() throws Exception {
        try(FileReader reader = new FileReader(name + ".property")) {
            Properties properties = new Properties();
            properties.load(reader);
            return readProperties(properties);
        }
    }

    private T readProperties(Properties properties) throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException {
        T instance = tClass.newInstance();

        for(String propName : properties.stringPropertyNames()) {
            String methodName = "set" + propName;

            Method method = getMethod(methodName);
            if(method == null) continue;

            Class[] paramTypes = method.getParameterTypes();

            if(paramTypes.length != 1) continue;

            Class type = paramTypes[0];
            Object val = readFromString(properties.getProperty(propName), type);

            method.invoke(instance, val);
        }

        return instance;
    }

    private Object readFromString(String str, Class type) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        if(type == String.class) {
            return str;
        }

        Object inst = Array.get(Array.newInstance(type, 1), 0);
        Method construct = inst.getClass().getMethod("valueOf", String.class);
        return construct.invoke(inst, str);
    }

    private Method getMethod(String name) {
        for(Method m : tClass.getMethods()) {
            if(m.getName().equals(name)) return m;
        }

        return null;
    }
}
