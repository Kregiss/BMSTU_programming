/*
Класс целочисленных матриц размера m n с порядком на основе
величины седловой точки. (Седловая точка – элемент матрицы,
одновременно наименьший в своей строки и наибольший в своём
столбце). Если матрица не имеет седловой точки, считать величину
седловой точки равной максимальному целому числу.
 */

package Lab3;

import java.util.Arrays;
public class Matrix implements Comparable<Matrix>{

    private int[][] matrix;
    public Matrix(int[][] arr) { this.matrix = arr; }

    public int getSaddlPoint(int[][] matrix) {
        int saddle = Integer.MAX_VALUE;
        boolean hadSaddlPoint = false;

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[i].length; j++) {
                int nowPoint = matrix[i][j];
                boolean isSaddlPoint = true;

                // проверяем наименьшесть в строке
                for (int k = 0; k < matrix[i].length; k++) {
                    if (nowPoint > matrix[i][k]) {
                        isSaddlPoint = false;
                        break;
                    }
                }

                // проверяем наибольшесть в столбце
                for (int p = 0; p < matrix.length; p++) {
                    if (nowPoint < matrix[p][j]) {
                        isSaddlPoint = false;
                        break;
                    }
                }

                if (isSaddlPoint) {
                    saddle = Math.min(saddle, nowPoint);
                    hadSaddlPoint = true;
                }
            }
        }

        if (!hadSaddlPoint) saddle = Integer.MAX_VALUE;

        return saddle;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int[] row : matrix) {
            sb.append(Arrays.toString(row)).append("\n");
        }
        System.out.println("Седловая точка:" + getSaddlPoint(this.matrix));
        return sb.toString() ;
    }

    public int compareTo(Matrix obj) {
        int thisSaddle = getSaddlPoint(this.matrix);
        int objSaddle = getSaddlPoint(obj.matrix);

        if (thisSaddle == objSaddle) return 0;
        else if (thisSaddle < objSaddle) return -1;
        else return 1;
    }
}
