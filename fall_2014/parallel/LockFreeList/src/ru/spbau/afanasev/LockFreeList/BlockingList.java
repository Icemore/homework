package ru.spbau.afanasev.LockFreeList;

public class BlockingList<T extends Comparable<? super T>> implements Set<T>{
    private class Node {
        public T key;
        public Node next;

        public Node() {}

        public Node(T key) {
            this(key, null);
        }

        public Node(T key, Node next) {
            this.key = key;
            this.next = next;
        }
    }

    private class Position {
        public Node prev, cur;

        public Position(Node prev, Node cur) {
            this.prev = prev;
            this.cur = cur;
        }
    }

    final Node head, tail;
    final Object lock = new Object();

    public BlockingList() {
        head = new Node();
        tail = new Node();

        head.next = tail;
    }

    @Override
    public boolean add(T key) {
        synchronized (lock) {
            Position pos = lowerBound(key);

            if(compare(pos.cur, key) == 0) {
                return false;
            }

            pos.prev.next = new Node(key, pos.cur);

            return true;
        }
    }

    @Override
    public boolean remove(T key) {
        synchronized (lock) {
            Position pos = lowerBound(key);

            if(compare(pos.cur, key) != 0) {
                return false;
            }

            pos.prev.next = pos.cur.next;
            return true;
        }
    }

    @Override
    public boolean contains(T key) {
        synchronized (lock) {
            Position pos = lowerBound(key);

            return compare(pos.cur, key) == 0;
        }
    }


    Position lowerBound(T key) {
        Node prev = head, cur = head.next;

        while(compare(cur, key) < 0) {
            prev = cur;
            cur = cur.next;
        }

        return new Position(prev, cur);
    }

    int compare(Node node, T key) {
        // Head < anything
        if(node == head) {
            return -1;
        }

        // Tail > anything
        if(node == tail) {
            return 1;
        }

        return node.key.compareTo(key);
    }
}
