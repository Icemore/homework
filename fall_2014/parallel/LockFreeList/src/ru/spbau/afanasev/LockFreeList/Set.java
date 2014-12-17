package ru.spbau.afanasev.LockFreeList;


public interface Set<T extends Comparable<? super T>> {
    public boolean add(T key);
    public boolean remove(T key);
    public boolean contains(T key);
}
