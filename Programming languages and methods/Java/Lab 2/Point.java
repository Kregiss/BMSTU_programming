public class Point {

    private String name;
    private int mass, speed;
    private int kinEnergy;
    public Point(String name, int mass, int speed)
    {
        this.name = name;
        this.mass = mass;
        this.speed = speed;
        this.kinEnergy = (mass * speed * speed) / 2;
    }

    public int getEnergy(){
        return kinEnergy;
    }

    public String toString() {
        return "("+name+", "+mass+", "+speed+", "+kinEnergy+")";
    }
}
