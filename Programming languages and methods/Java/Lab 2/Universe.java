import java.util.Arrays;
public class Universe {

    private Point [] universe;
    private int count = 0;
    private int sumEnergy;

    public Universe()
    {
        Point [] universe = new Point[3];
        universe[0] = new Point("A",100,100);
        System.out.println(universe[0]);
        universe[1] = new Point("B",101,101);
        System.out.println(universe[1]);
        universe[2] = new Point("C",102,102);
        System.out.println(universe[2]);

        this.universe = universe;
        count = universe.length;
    }

    public int getSumKinEnergy()
    {
        for(int i = 0; i < universe.length; i++){
            this.sumEnergy += universe[i].getEnergy();
        }
        return this.sumEnergy;
    }

    public int getSumEnergy(){ return sumEnergy; }

    public String toString() {
        return "("+Arrays.toString(universe)+", \n"+count+", \n"+sumEnergy+")";
    }
}
