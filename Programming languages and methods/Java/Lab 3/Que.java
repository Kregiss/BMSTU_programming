/*
Класс очередей целых чисел с порядком на основе суммы элементов
очереди.
 */

package Lab3_2;

import java.util.LinkedList;

import java.util.Queue;

public class Que implements Comparable<Que>{

    private Queue<Integer> queue;

    public Que() {
        this.queue = new LinkedList<>();
    }

    public void enqueue(int element) {
        queue.offer(element);
    }

    public int sum() {
        int sum = 0;
        for (int element : queue) {
            sum += element;
        }
        return sum;
    }
    public String toString() {
        return queue.toString();
    }

    public int compareTo(Que obj) {
        int thisSum = this.sum();
        int objSum= obj.sum();

        if (thisSum == objSum) return 0;
        else if (thisSum < objSum) return -1;
        else return 1;
    }
}
