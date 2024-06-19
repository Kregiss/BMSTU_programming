package main

/*
решение на java

совет: 
дважды прогнать алгоритм - если оценки максимальных путей 
увеличились, значит бесконечен
*/

/*
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class Vertex {
    int vertnum;
    String sost;
    long dist;
    Vertex parent;
    LinkedVertices next;

    Vertex(int vertnum, String sost) {
        this.vertnum = vertnum;
        this.sost = sost;
        this.dist = Long.MAX_VALUE;
        this.parent = null;
        this.next = null;
    }
}

class LinkedVertices {
    int vertnum;
    int val;
    LinkedVertices next;

    LinkedVertices(int vertnum, int val) {
        this.vertnum = vertnum;
        this.val = val;
        this.next = null;
    }
}

public class LongWord {
    public static void insert(Vertex l, LinkedVertices x) {
        LinkedVertices cur = l.next;
        if (cur == null) {
            l.next = x;
        } else {
            while (cur.next != null) {
                cur = cur.next;
            }
            cur.next = x;
        }
    }

    public static boolean relax(Vertex v, Vertex u, int w) {
        boolean changed = v.dist + w < u.dist;
        if (changed) {
            u.dist = v.dist + w;
        }
        return changed;
    }

    public static boolean bellmanford(List<Vertex> graph, int q0) {
        for (Vertex v : graph) {
            v.dist = Long.MAX_VALUE;
            v.parent = null;
        }
        graph.get(q0).dist = 0;

        for (int i = 1; i < graph.size(); i++) {
            for (int k = 0; k < graph.size(); k++) {
                Vertex cur = graph.get(k);
                for (LinkedVertices j = cur.next; j != null; j = j.next) {
                    relax(cur, graph.get(j.vertnum), j.val);
                }
            }
        }

        for (int i = 0; i < graph.size(); i++) {
            Vertex cur = graph.get(i);
            for (LinkedVertices j = cur.next; j != null; j = j.next) {
                if (relax(cur, graph.get(j.vertnum), j.val) && graph.get(j.vertnum).sost.equals("+")) {
                    return true;
                }
            }
        }

        return false;
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int M = scanner.nextInt();
        int N = scanner.nextInt();
        int q0 = scanner.nextInt();
        scanner.nextLine(); 

        List<Vertex> graph = new ArrayList<>();
        for (int i = 0; i < N; i++) {
            String sost = scanner.next();
            graph.add(new Vertex(i, sost));
            for (int j = 0; j < M; j++) {
                int v1 = scanner.nextInt();
                insert(graph.get(i), new LinkedVertices(v1, -1));
            }
            scanner.nextLine();
        }
        scanner.close();

        boolean err = bellmanford(graph, q0);
        if (err) {
            System.out.println("INF");
            return;
        }

        boolean isemp = true;
        for (int i = 0; i < N; i++) {
            if (graph.get(i).sost.equals("+") && i != q0) {
                isemp = false;
            }
        }

        if (isemp && graph.get(q0).sost.equals("+")) {
            System.out.println(0);
            return;
        } else if (isemp && graph.get(q0).sost.equals("-")) {
            System.out.println("EMPTY");
            return;
        }

        long res = 0;
        for (int i = 0; i < N; i++) {
            Vertex cur = graph.get(i);
            if (-cur.dist > res && cur.sost.equals("+")) {
                res = -cur.dist;
            }
        }

        System.out.println(res);
    }
}
*/

/*
3 8 5
+ 3 6 6
+ 2 7 4
+ 3 0 6
- 3 3 3
+ 7 6 6
+ 3 0 1
+ 3 3 3
+ 0 2 0

6		// ccabbb


4 34 15
- 22 10 4 24
- 2 32 30 30
- 20 22 23 17
- 26 32 2 6
- 4 4 23 4
- 32 7 11 4
- 4 32 17 16
- 20 26 29 6
- 27 31 13 28
- 14 15 4 18
- 24 6 2 20
- 20 4 23 19
- 0 21 29 18
- 12 14 0 0
- 17 21 3 7
- 3 25 27 27
- 19 3 21 31
- 17 17 17 17
- 29 22 10 11
- 19 17 19 19
- 20 20 21 20
- 21 21 21 21
- 19 11 20 4
- 23 23 23 23
- 4 33 30 23
- 15 8 8 7
- 20 15 22 5
- 22 17 33 29
- 9 4 16 1
- 7 1 24 5
+ 21 22 32 4
- 18 5 4 2
- 4 11 33 21
- 19 11 4 17

INF
*/