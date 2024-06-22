/*
Последовательность трёхмерных векторов с
итератором по векторным произведениям
соседних векторов.
 */

package Vector_51;

import java.util.Iterator;

public class VectorList implements Iterable {

    private Vector3[] vectors;

    public VectorList(Vector3[] vectors) { this.vectors = vectors; }

    public void addVector(Vector3 newVector) {
        Vector3[] newVectors = new Vector3[this.vectors.length + 1];
        for (int i = 0; i < this.vectors.length; i++) {
            newVectors[i] = vectors[i];
        }
        newVectors[this.vectors.length] = newVector;
        vectors = newVectors;
    }

    public Iterator iterator() { return new VectorIterator(); }

    private class VectorIterator implements Iterator {
        private int pos = 0;
        public boolean hasNext () { return pos < vectors.length - 1; }
        public Vector3 next () {
            Vector3 current = vectors[pos], next = vectors[pos + 1];
            pos++;
            return current.vectorProduct(next);
        }
    }
}
