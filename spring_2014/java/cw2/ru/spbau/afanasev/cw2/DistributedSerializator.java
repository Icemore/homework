package ru.spbau.afanasev.cw2;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class DistributedSerializator<T> {
    private ExecutorService executorService;
    private Class<T> tClass;

    public DistributedSerializator(Class<T> tClass) {
        this.tClass = tClass;
        executorService = Executors.newFixedThreadPool(5);
    }

    public void shutdown() {
        executorService.shutdown();
    }

    public void serialize(T o, String name) throws InterruptedException {
        Future<Void> result = executorService.submit(new SerializeTask<T>(o, name));

        try {
            result.get();
        } catch (ExecutionException e) {
            throw new InterruptedException(e.getMessage());
        }
    }

    public T deserialize(String name) throws InterruptedException{
        Future<T> result = executorService.submit(new DeserializeTask<T>(tClass, name));

        try {
            return result.get();
        } catch (ExecutionException e) {
            throw new InterruptedException(e.getMessage());
        }
    }
}
