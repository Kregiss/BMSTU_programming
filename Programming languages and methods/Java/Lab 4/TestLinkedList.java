/*
Обобщённый однонаправленный связанный
список с итератором по значениям, хранящимся в
его элементах.
 */

package LinkedList_36;

public class Test {
    public static void main(String[] args) {

        LinkedList<Integer> ls = new LinkedList<>();
        ls.addLinkedList(1);
        ls.addLinkedList(6);
        ls.addLinkedList(2);
        ls.addLinkedList(7);
        ls.addLinkedList(3);

        for (Integer value : ls) { System.out.print(value + ", "); }
        System.out.print("\n");


        ls.addLinkedList(10);
        System.out.println("New: ");
        for (Integer value : ls) { System.out.print(value + ", "); }
    }
}
