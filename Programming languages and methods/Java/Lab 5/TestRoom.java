/*
Множество комнат офисного центра, про каждую из которых известен
её номер и арендатор, причём у части комнат арендатор может отсутствовать.
Номера комнат – трёхзначные, при этом старшая цифра обозначает номер этажа.

Операции:
1. порождение потока номеров комнат, арендуемых указанной фирмой;

2. вычисление максимального количества комнат на этаже, в котором все комнаты – пустые.

Проверить работу первой операции нужно путём вычисления количества комнат, снимаемых
некоторой фирмой, на каждом этаже.
 */

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class RoomOffice {
    HashMap<Integer, String> Office;

    RoomOffice () {
        Office = new HashMap<>();
    }

    void add(int number, String name) {
        Office.put(number, name);
    }
    void add(int number) {
        Office.put(number, "");
    }

    /*
        1. Порождение потока номеров комнат, арендуемых указанной фирмой;
     */
    public Stream<Integer> officeStream(String v) {
        ArrayList<Integer> result = new ArrayList<>();
        Office
                .entrySet()
                .stream()
                .filter(x -> v.equals(x.getValue()))
                .forEach(x -> result.add(x.getKey()));
        return result.stream();
    }

    /*
    Проверить работу первой операции нужно путём вычисления количества комнат,
    снимаемых некоторой фирмой, на каждом этаже.
    */
    public Map<Integer, Long> countRoomsOnFloor (String nameCompany) {
        Map<Integer, Long> rooms = Office
                .entrySet()
                .stream()
                .filter(x -> x.getValue().equals(nameCompany))
                .map(x -> x.getKey() / 100)
                .collect(Collectors.groupingBy(floor -> floor, Collectors.counting()));
        return rooms;
    }

    /*
        2. Вычисление максимального количества комнат на этаже,
        в котором все комнаты – пустые.
     */

    // Метод, возвращающий этажи со всеми пустыми комнатами
    public Set<Integer> floorsWithAllEmptyRooms() {
        Map<Integer, Long> emptyRoomsByFloor = Office
                .keySet()
                .stream()
                .collect(Collectors.groupingBy(
                        roomNumber -> roomNumber / 100, // Группируем по этажам
                        Collectors.counting()
                ));

        Set<Integer> allEmptyFloors = emptyRoomsByFloor
                .entrySet()
                .stream()
                .filter(entry -> Office
                        .keySet()
                        .stream()
                        .noneMatch(roomNumber -> roomNumber / 100 == entry.getKey() && !Office.get(roomNumber).isEmpty()))
                        // на текущем этаже нет ни одной комнаты, у которой есть арендатор
                .map(Map.Entry::getKey)
                .collect(Collectors.toSet());

        return allEmptyFloors;
    }

    // Метод, находящий этаж с максимальным количеством комнат среди всех
    // этажей со всеми пустыми комнатами
    public Optional<Long> maxEmptyRoomsFloor() {
        Set<Integer> allEmptyFloors = floorsWithAllEmptyRooms();
        int maxEmptyRoomCount = 0;
        // Найдем этаж с максимальным количеством пустых комнат
        Optional<Integer> maxFloor = allEmptyFloors
                .stream()
                .max(new FloorComparator(Office));

        // Вернём количество комнат на найденном этаже
        return maxFloor.map(floor -> {
            long roomCount = Office.keySet().stream()
                    .filter(roomNumber -> roomNumber / 100 == floor)
                    .count();
            //System.out.println("Количество комнат на " + floor + " этаже: " + roomCount);
            return roomCount;
        });
    }
}

class FloorComparator implements Comparator<Integer> {
    private final Map<Integer, String> office;

    FloorComparator(Map<Integer, String> office) {
        this.office = office;
    }

    public int compare(Integer floor1, Integer floor2) {
        long count1 = office.keySet().stream()
                .filter(roomNumber -> roomNumber / 100 == floor1)
                .count();
        long count2 = office.keySet().stream()
                .filter(roomNumber -> roomNumber / 100 == floor2)
                .count();
        return Long.compare(count1, count2);
    }
}

public class Test {
    public static void main(String[] args) {
        RoomOffice officeCenter = new RoomOffice();
        officeCenter.add(101);
        officeCenter.add(110, "delta");
        officeCenter.add(111);
        officeCenter.add(170, "alpha");
        officeCenter.add(250, "alpha");
        officeCenter.add(261, "alpha");
        officeCenter.add(268, "gamma");
        officeCenter.add(301);
        officeCenter.add(311);
        officeCenter.add(340);
        officeCenter.add(387);
        officeCenter.add(501, "gamma");
        officeCenter.add(540);
        officeCenter.add(587, "gamma");
        officeCenter.add(652, "beta");
        officeCenter.add(687, "gamma");
        officeCenter.add(701);
        officeCenter.add(740);
        officeCenter.add(911);


        String targetCompany = "gamma";
        System.out.println("--1--Порождение потока номеров комнат,\nарендуемых указанной фирмой: ");
        officeCenter.officeStream(targetCompany).forEach(System.out::println);
        Map<Integer, Long> rooms = officeCenter.countRoomsOnFloor(targetCompany);
        System.out.println("Количество комнат, снимаемых фирмой " + targetCompany + " на каждом этаже: ");
        rooms.forEach((floor, count) -> System.out.println("Этаж " + floor + ": " + count));

        System.out.println("--2--Вычисление максимального количества комнат\nна этаже, в котором все комнаты – пустые: ");
        System.out.println(officeCenter.maxEmptyRoomsFloor().get());
        // Answer: 3 floor - 4 rooms
    }
}
