package Vector_51;

public class Vector3 {
    private double x, y, z;

    public Vector3(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public double getX() { return this.x; }
    public double getY() { return this.y; }
    public double getZ() { return this.z; }

    public Vector3 vectorProduct(Vector3 other) {
        double newX = this.y * other.getZ() - this.z * other.getY();
        double newY = this.z * other.getX() - this.x * other.getZ();
        double newZ = this.x * other.getY() - this.y * other.getX();
        return new Vector3(newX, newY, newZ);
    }

    public String toString() {
        return "(" + x + ", " + y + ", " + z + ")";
    }

}
