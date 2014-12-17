package ru.spbau.afanasev.LockFreeList;


import java.util.concurrent.atomic.AtomicMarkableReference;

public class LockFreeList<T extends Comparable<? super T>> implements Set<T> {
    private class Node {
        public T key;
        public AtomicMarkableReference<Node> next;

        public Node() {}
        public Node(T key) {
            this.key = key;
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

    public LockFreeList() {
        head = new Node();
        tail = new Node();

        head.next = new AtomicMarkableReference<>(tail, false);
        tail.next = new AtomicMarkableReference<>(null, false);
    }

    @Override
    public boolean add(T key) {
        while(true) {
            Position pos = lowerBound(head, key);

            if(compare(pos.cur, key) == 0) {
                return false;
            }

            Node newNode = new Node(key);
            newNode.next = new AtomicMarkableReference<>(pos.cur, false);
            if(pos.prev.next.compareAndSet(pos.cur, newNode, false, false)) {
                return true;
            }
        }
    }

    @Override
    public boolean remove(T key) {
        while(true) {
            Position pos = lowerBound(head, key);

            if(compare(pos.cur, key) != 0) {
                return false;
            }

            Node next = pos.cur.next.getReference();
            boolean cas = pos.cur.next.attemptMark(next, true);

            if(cas) {
                pos.prev.next.compareAndSet(pos.cur, next, false, false);
                return true;
            }
        }
    }

    @Override
    public boolean contains(T key) {
        boolean[] marked = {false};

        Node cur = head;
        while(compare(cur, key) < 0) {
            cur = cur.next.getReference();
            cur.next.get(marked);
        }

        return (compare(cur, key) == 0) && !marked[0];
    }

    private Position lowerBound(Node head, T key) {
        Node prev, cur, next;
        boolean[] marked = {false};

        retry: while(true) {
            prev = head;
            cur = prev.next.getReference();

            while(true) {
                next = cur.next.get(marked);
                while(marked[0]) {
                    boolean cas = prev.next.compareAndSet(cur, next, false, false);
                    if(!cas) {
                        continue retry;
                    }

                    cur = next;
                    next = cur.next.get(marked);
                }

                if(compare(cur, key) >= 0) {
                    return new Position(prev, cur);
                }

                prev = cur;
                cur = next;
            }
        }
    }


    private int compare(Node node, T key) {
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
