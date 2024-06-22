/*
Класс, представляющий успеваемость группы студентов
по некоторому предмету, с операцией, вычисляющей
оценку указанного студента. (Если отсортировать
студентов по убыванию баллов, то первые 25% получают
5, следующие 25% – 4, и т.д.)
 */
public class Test {

    public static void main(String[] args) {

        Group A = new Group("A");
        A.students[0] = 654;
        A.students[1] = 434;
        A.students[2] = 456;
        A.students[3] = 897;
        A.students[4] = 126;
        A.students[5] = 847;
        A.students[6] = 455;
        A.students[7] = 167;

        A.studNames[0] = "Антон";
        A.studNames[1] = "Борис";
        A.studNames[2] = "Клара";
        A.studNames[3] = "Денис";
        A.studNames[4] = "Елисей";
        A.studNames[5] = "Иван";
        A.studNames[6] = "Георгий";
        A.studNames[7] = "Харитон";

        System.out.println(A);

        A.nameStud = "Клара";
        A.scoreSort();
        A.finishScore();

        System.out.println(A);

        System.out.println("Оценка студента " + A.nameStud + ": " + A.getFinScore());
    }
}
