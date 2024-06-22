/*
Множество чисел, представленных объектами
класса BigInteger, с операциями:
1. порождение потока, составленного из тех чисел
множества, которые точно не являются простыми
(см. метод isProbablePrime), отсортированного по
возрастанию;
2. поиск числа, который делится на все остальные
числа множества без остатка;
3. добавление числа.
Проверить работу второй операции нужно путём
ранжирования чисел по длине в битах.
 */

import java.util.*;
import java.util.stream.Stream;
import java.math.BigInteger;

class BigInterSet {
    private List<BigInteger> numbers;
    public BigInterSet() {
        numbers = new ArrayList<>();
    }

    /*
        3. добавление числа.
     */
    public void addNumber(BigInteger number) {
        numbers.add(number);
    }

    /*
        1. порождение потока, составленного из тех чисел
        множества, которые точно не являются простыми
        (см. метод isProbablePrime), отсортированного по
        возрастанию;
     */
    public Stream<BigInteger> nonPrimeNumbersStream() {
        ArrayList<BigInteger> result = new ArrayList<>();
        numbers
                .stream()
                .filter(x -> !x.isProbablePrime(100))
                .sorted()
                .forEach(x -> result.add(x));
        return result.stream();
    }

    /*
        2. поиск числа, которое делится на все остальные
        числа множества без остатка;
     */
    public Optional<BigInteger> findDivisibleNumber() {
        Optional<BigInteger> result = Optional.empty();
        Optional<BigInteger> tmp = numbers
                .stream()
                .filter(num -> numbers
                        .stream()
                        .allMatch(divisor -> divisor.equals(num) || num.remainder(divisor).equals(BigInteger.ZERO)))
                .findFirst();
        if (tmp.isPresent()) { // проверка на непустоту
            result = Optional.of(tmp.get());
        }
        return result;
    }
}

class BigIntComparator implements Comparator<BigInteger> {
    public int compare(BigInteger a, BigInteger b) {
        int bitsA = a.bitCount();
        int bitsB = b.bitCount();

        if (bitsA > bitsB) {
            return 1;
        } else if (bitsA < bitsB) {
            return -1;
        } else {
            return 0;
        }
    }
}

public class Test {
    public static void main(String[] args) {

        BigInteger[] numbers = {
                BigInteger.valueOf(13),
                BigInteger.valueOf(11),
                BigInteger.valueOf(143),
                BigInteger.valueOf(11),
                BigInteger.valueOf(84),
                BigInteger.valueOf(47),
                BigInteger.valueOf(107)
        };
        BigInteger lcm = numbers[0];
        for (int i = 1; i < numbers.length; i++) {
            lcm = lcm.multiply(numbers[i]).divide(lcm.gcd(numbers[i]));
        }
        System.out.println("Число, которое делится на все перечисленные числа: " + lcm);

        BigInterSet s = new BigInterSet();
        s.addNumber(BigInteger.valueOf(13));
        s.addNumber(BigInteger.valueOf(11));
        s.addNumber(BigInteger.valueOf(143)); // 11 * 13
        s.addNumber(BigInteger.valueOf(60408348));
        s.addNumber(BigInteger.valueOf(84));
        s.addNumber(BigInteger.valueOf(47));
        s.addNumber(BigInteger.valueOf(107));
        s.addNumber(BigInteger.valueOf(11));
        s.addNumber(BigInteger.valueOf(60408348));
        System.out.println("--1--Порождение потока, составленного из тех чисел множества,  \n" +
                "которые точно не являются простыми, отсортированного по возрастанию: ");
        s.nonPrimeNumbersStream().sorted(new BigIntComparator()).forEach(System.out::println);
        System.out.println("--2--Поиск числа, которое делится на все остальные \n" +
                "числа множества без остатка: ");
        System.out.println(s.findDivisibleNumber().get());


    }
}
