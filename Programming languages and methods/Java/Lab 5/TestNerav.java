/*
Множество неравенств вида x > a, где x – имя переменной,
a a – целое число, с операциями:

1. порождение на основе множества неравенств потока имён переменных,
которые могут принимать только положительные значения;

2. вычисление минимального значения указанной переменной,
удовлетворяющего всем неравенствам, в которые она входит.

Проверить работу первой операции нужно путём группировки имён
переменных по первой букве.
 */

import java.util.*;
import java.util.stream.Stream;

class Neravenstvo {
    String name;
    int number;

    Neravenstvo (String name, int number) {
        this.name = name;
        this.number = number;
    }
}

class NeravTable {
    HashMap<String, Neravenstvo> Table;

    NeravTable () {
        Table = new HashMap<>();
    }

    /*
    при добавлении нового неравенства проверяем:
    есть ли уже неравенство с этой переменной в HashMap, и если есть,
    то оставлять большее значение number
     */
    void add(String name, int number) {
        if (Table.containsKey(name)) {
            if (number > Table.get(name).number) {
                Table.put(name, new Neravenstvo(name, number));
            }
        } else {
            Table.put(name, new Neravenstvo(name, number));
        }
    }

    public Stream<String> nameStream() {
        ArrayList<String> result = new ArrayList<>();
        Table
                .entrySet()
                .stream()
                .filter(x -> x.getValue().number > 0)
                .forEach(x -> result.add(x.getValue().name));
        return result.stream();
    }

    public Optional<Integer> getNerav(String v) {
        Optional<Integer> result = Optional.empty();
        Optional<Map.Entry<String, Neravenstvo>> tmp = Table
                .entrySet()
                .stream()
                .filter(x -> x.getValue().name.equals(v))
                .findFirst();
        if (tmp.isPresent()) { // проверка на непустоту
            result = Optional.of(tmp.get().getValue().number);
        }
        return result;
    }
}

class NeravComparator implements Comparator<String> {
    public int compare(String a, String b) {
        char a0, b0;
        a0 = a.charAt(0);
        b0 = b.charAt(0);
        if (a0 > b0) { return 1; }
        if (a0 == b0) { return 0; }
        return -1;
    }
}

public class Test {
    public static void main(String[] args) {
        NeravTable t = new NeravTable();
        t.add("a", -10);
        t.add("x", 6);
        t.add("x", -7);
        t.add("c", 2);
        t.add("a", 18);
        t.add("d", -6);
        t.add("d", -10);
        t.add("a", -6);
        t.add("x", 0);
        System.out.println("--1--Порождение на основе множества неравенств потока имён переменных, \n" +
                "которые могут принимать только положительные значения: ");
        t.nameStream().sorted(new NeravComparator()).forEach(System.out::println);
        System.out.println("--2--Вычисление минимального значения указанной переменной, \n" +
                        "удовлетворяющего всем неравенствам, в которые она входит: ");
        System.out.println(t.getNerav("a").get());
    }
}
