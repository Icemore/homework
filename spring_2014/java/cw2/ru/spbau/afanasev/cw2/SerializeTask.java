package ru.spbau.afanasev.cw2;

import java.io.FileWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.concurrent.Callable;

public class SerializeTask<T> implements Callable<Void> {
    T object;
    String name;

    public SerializeTask(T object, String name) {
        this.object = object;
        this.name = name;
    }

    @Override
    public Void call() throws Exception {
        Properties properties = getProperties(object);

        try(FileWriter writer = new FileWriter(name + ".property")) {
            properties.store(writer, name);
        }

        return null;
    }

    private Properties getProperties(T o) {
        Properties properties = new Properties();

        for(Method method : o.getClass().getMethods()) {
            if(method.getName().startsWith("get") &&
                    method.getParameterTypes().length == 0) {

                String propName = method.getName().substring(3);
                Object val;

                try {
                    val = method.invoke(o);
                } catch (IllegalAccessException | InvocationTargetException e) {
                    throw new RuntimeException();
                }

                properties.setProperty(propName, val.toString());
            }
        }

        return properties;
    }
}
