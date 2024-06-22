/*
Класс очередей целых чисел с порядком на основе суммы элементов
очереди.
 */

package Lab3_2;

import java.util.Arrays;


public class Test {
    public static void main(String[] args) {


        Que queue1 = new Que();
        queue1.enqueue(1);
        queue1.enqueue(2);
        queue1.enqueue(3);

        Que queue2 = new Que();
        queue2.enqueue(3);
        queue2.enqueue(4);
        queue2.enqueue(5);

        Que queue3 = new Que();
        queue3.enqueue(5);
        queue3.enqueue(6);
        queue3.enqueue(7);

        Que[] a = {queue3, queue2, queue1};

        System.out.println("Исходный массив:");
        for (Que x : a) System.out.println(x);

        System.out.println("Конечный массив:");
        Arrays.sort(a);
        for (Que x : a) System.out.println(x);
    }
}
