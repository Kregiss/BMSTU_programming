/*
Класс целочисленных матриц размера m n с порядком на основе
величины седловой точки. (Седловая точка – элемент матрицы,
одновременно наименьший в своей строки и наибольший в своём
столбце). Если матрица не имеет седловой точки, считать величину
седловой точки равной максимальному целому числу.
 */

package Lab3;

import java.util.Arrays;

public class TestMatrix {
    public static void main(String[] args) {

        Matrix[] a = {
                new Matrix(new int[][]{{61, 52, 85}, {82, 60, 10}}),
                new Matrix(new int[][]{{42, 84, 21}, {87, 94, 41}}),
                new Matrix(new int[][]{{54, 203, 105}, {37, 43, 71}}),
        };

        Arrays.sort(a);
        for (Matrix x : a) System.out.println(x);
    }
}
