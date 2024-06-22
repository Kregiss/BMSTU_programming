import java.util.Arrays;

public class Group {
    private String nameGroup;
    private int finScore;
    public String nameStud;
    public int [] students = new int[8];
    public String [] studNames = new String[8];

    public Group(String nameGroup)
    {
        this.nameGroup = nameGroup;
    }

    public void scoreSort(){
        for (int i = 0; i < students.length - 1; i++) {
            for(int j = 0; j < students.length - i - 1; j++) {
                if(students[j + 1] < students[j]) {
                    int swap = students[j];
                    students[j] = students[j + 1];
                    students[j + 1] = swap;

                    String swapName = studNames[j];
                    studNames[j] = studNames[j + 1];
                    studNames[j + 1] = swapName;
                }
            }
        }
    }

    public int finishScore()
    {
        for (int i = 0; i < 8; i++) {
            if (studNames[i].equals(this.nameStud)) {
                if (i >= 0.75 * 8) {
                    this.finScore = 5;
                } else if (i >= 0.5 * 8) {
                    this.finScore = 4;
                } else if (i >= 0.25 * 8) {
                    this.finScore = 3;
                } else {
                    this.finScore = 2;
                }
            }
        }
        return this.finScore;
    }

    public int getFinScore(){
        return this.finScore;
    }

    public String toString() {
        return "("+Arrays.toString(studNames)+", \n"+Arrays.toString(students)+")";
    }
}
