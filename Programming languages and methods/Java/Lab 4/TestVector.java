/*
Последовательность трёхмерных векторов с
итератором по векторным произведениям
соседних векторов.
 */

package Vector_51;

public class Test {
    public static void main(String[] args) {
        Vector3[] vectors = new Vector3[]{
                new Vector3(1, 29, 3),
                new Vector3(4, 5, 6),
                new Vector3(10, 8, 9)
        };

        VectorList vectorList = new VectorList(vectors);

        for (Object vector : vectorList) {
            System.out.println(vector);
        }

        vectorList.addVector(new Vector3(5, 9, 7));
        System.out.println("New: ");
        for (Object vector : vectorList) {
            System.out.println(vector);
        }
    }
}
