package main
/*
import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Edge struct {
    node  int
    color int
}

type Room struct {
    connections [][]Edge
}

type Item struct {
    node      int
    cost      int
    path      []int
    index     int
}

type PriorityQueue []*Item

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
    if pq[i].cost == pq[j].cost {
        return lexicographicallyLess(pq[i].path, pq[j].path)
    }
    return pq[i].cost < pq[j].cost
}

func lexicographicallyLess(path1, path2 []int) bool {
    minLen := len(path1)
    if len(path2) < minLen {
        minLen = len(path2)
    }
    for i := 0; i < minLen; i++ {
        if path1[i] != path2[i] {
            return path1[i] < path2[i]
        }
    }
    return len(path1) < len(path2)
}

func (pq PriorityQueue) Swap(i, j int) {
    pq[i], pq[j] = pq[j], pq[i]
    pq[i].index = i
    pq[j].index = j
}

func (pq *PriorityQueue) Push(x interface{}) {
    n := len(*pq)
    item := x.(*Item)
    item.index = n
    *pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
    old := *pq
    n := len(old)
    item := old[n-1]
    item.index = -1
    *pq = old[0 : n-1]
    return item
}

func Dijkstra(start, end int, rooms []*Room) (int, []int) {
    if start < 0 || start >= len(rooms) || end < 0 || end >= len(rooms) {
        return -1, nil
    }

    minPathLen := make(map[int]int) 
    pq := make(PriorityQueue, 0)
    heap.Init(&pq)

    heap.Push(&pq, &Item{node: start, cost: 0, path: []int{}})

    for i := 0; i < len(rooms); i++ {
        minPathLen[i] = -1
    }

    for pq.Len() > 0 {
        item := heap.Pop(&pq).(*Item)

        if item.node == end {
            return item.cost, item.path
        }

        if minPathLen[item.node] != -1 && len(item.path) >= minPathLen[item.node] {
            continue
        }

        minPathLen[item.node] = len(item.path)

        for _, connection := range rooms[item.node].connections {
            for _, edge := range connection {
                neighbor, color := edge.node, edge.color
                newPath := append([]int(nil), item.path...)
                newPath = append(newPath, color)
                heap.Push(&pq, &Item{node: neighbor, cost: item.cost + 1, path: newPath})
            }
        }
    }

    return -1, nil
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	scanner.Scan()
	params := strings.Fields(scanner.Text())
	n, _ := strconv.Atoi(params[0])
	m, _ := strconv.Atoi(params[1])

	rooms := make([]*Room, n+1)
	for i := range rooms {
		rooms[i] = &Room{}
	}

	for i := 0; i < m; i++ {
		scanner.Scan()
		connection := strings.Fields(scanner.Text())
		a, _ := strconv.Atoi(connection[0])
		b, _ := strconv.Atoi(connection[1])
		c, _ := strconv.Atoi(connection[2])
		rooms[a].connections = append(rooms[a].connections, []Edge{{b, c}})
		rooms[b].connections = append(rooms[b].connections, []Edge{{a, c}})
	}

	shortestCost, shortestPath := Dijkstra(1, n, rooms)
	fmt.Println(shortestCost)
	fmt.Println(strings.Trim(strings.Join(strings.Fields(fmt.Sprint(shortestPath)), " "), "[]"))
}
*/

/*
4 6
1 2 1
1 3 2
3 4 3
2 3 1
2 4 4
3 1 1

2
1 3


20 20
13 2 2
19 20 3
15 2 5
18 8 4
3 14 5
10 20 4
16 13 5
19 3 4
11 12 6
13 1 1
4 3 3
13 9 1
9 16 6
17 4 6
17 7 2
8 6 5
5 8 6
15 6 4
18 7 6
12 16 4

12
1 2 5 4 5 4 6 2 6 3 4 3


50 100
48 20 9
17 2 6
7 12 8
26 3 4
37 30 2
41 44 9
6 45 7
7 18 8
12 13 10
12 38 9
9 34 6
39 48 7
38 1 3
26 13 3
25 22 10
12 34 5
15 12 10
34 16 1
7 14 10
8 30 11
37 21 1
2 19 7
22 16 2
45 26 5
8 27 8
8 21 1
47 34 1
32 19 5
29 45 11
10 43 1
9 40 8
6 2 2
2 42 4
9 48 5
37 42 7
32 22 12
40 14 3
9 6 4
49 50 7
10 45 2
31 40 4
26 28 4
42 48 8
24 15 2
39 41 3
26 21 6
28 15 8
31 19 12
8 29 7
2 14 12
1 1 6
40 6 8
29 21 10
17 10 2
40 46 12
3 6 9
11 3 2
4 50 3
9 2 9
50 47 11
28 31 11
41 4 9
13 29 1
33 22 4
4 33 5
28 37 6
37 11 7
37 41 10
28 16 3
42 20 3
17 35 10
8 44 9
19 33 7
22 17 3
38 25 1
42 22 9
28 23 7
1 5 10
19 3 11
18 22 4
39 17 12
39 21 8
2 46 1
46 49 9
12 15 4
32 29 12
12 17 10
9 38 6
33 41 2
3 26 9
47 48 2
45 43 10
4 50 9
37 50 1
33 24 12
44 35 10
37 4 11
41 33 4
22 36 11
50 14 3

5
3 6 5 2 11
*/