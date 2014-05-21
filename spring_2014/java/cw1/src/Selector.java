public interface Selector<T> {
    T current();

    boolean hasNext();

    void next();
}
