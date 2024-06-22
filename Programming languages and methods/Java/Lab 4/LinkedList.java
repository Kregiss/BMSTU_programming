/*
Обобщённый однонаправленный связанный
список с итератором по значениям, хранящимся в
его элементах.
 */

package LinkedList_36;

import java.util.Iterator;

class Node<T>{
    T data;
    Node<T> next;

    Node (T data) {
        this.data = data;
        this.next = null;
    }
}

public class LinkedList<T> implements Iterable<T>{
    private Node<T> head;
    private Node<T> tail;
    private int size;

    public LinkedList() {
        this.head = null;
        this.tail = null;
        this.size = 0;
    }

    public void addLinkedList(T data) {
        Node<T> newNode = new Node<>(data);
        if (head == null) {
            head = newNode;
            tail = newNode;
        } else {
            tail.next = newNode;
            tail = newNode;
        }
        size++;
    }

    public Iterator iterator() { return new LinkedIterator(); }

    private class LinkedIterator implements Iterator {
        private Node<T> current = head;
        public boolean hasNext() { return current != null; }
        public T next() {
            T data = current.data;
            current = current.next;
            return data;
        }
    }
}
