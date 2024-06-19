package main
/*
import (
	"container/heap"
	"fmt"
)

type Edge struct {
	u, v int
	len  int
}

type Vertex struct {
	index int
	key   int
	value int     // Предыдущая вершина
}

type PriorityQueue []*Vertex

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].key < pq[j].key }
func (pq PriorityQueue) Swap(i, j int)      { pq[i], pq[j] = pq[j], pq[i] }

func (pq *PriorityQueue) Push(x interface{}) {
	item := x.(*Vertex)
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	*pq = old[0 : n-1]
	return item
}

func decreaseKey(pq PriorityQueue, v *Vertex, key int) {
	for i := range pq {
		if pq[i] == v {
			pq[i].key = key
			heap.Fix(&pq, i)
			break
		}
	}
}

func MSTPrim(N int, edges []Edge) int {
	var totalLen int
	var pq PriorityQueue
	graph := make(map[int][]Edge)

	for _, e := range edges {
		graph[e.u] = append(graph[e.u], e)
		graph[e.v] = append(graph[e.v], Edge{u: e.v, v: e.u, len: e.len})
	}

	vertices := make([]*Vertex, N)
	for i := range vertices {
		vertices[i] = &Vertex{index: i, key: 1<<31 - 1}
	}

	startVertex := vertices[0]
	startVertex.key = 0

	heap.Init(&pq)
	for _, v := range vertices {
		heap.Push(&pq, v)
	}

	for pq.Len() != 0 {
		u := heap.Pop(&pq).(*Vertex)

		if u.value != -1 {
			totalLen += u.key
		}

		for _, e := range graph[u.index] {
			v := vertices[e.v]
			if contains1(pq, v) && e.len < v.key {
				v.value = u.index
				decreaseKey(pq, v, e.len)
			}
		}
	}

	return totalLen
}

func contains1(pq PriorityQueue, v *Vertex) bool {
	for _, item := range pq {
		if item == v {
			return true
		}
	}
	return false
}

func main() {
	var N, M int
	fmt.Scan(&N, &M)

	edges := make([]Edge, M)
	for i := 0; i < M; i++ {
		var u, v, length int
		fmt.Scan(&u, &v, &length)
		edges[i] = Edge{u: u, v: v, len: length}
	}

	minTotalLength := MSTPrim(N, edges)
	fmt.Println(minTotalLength)
}
*/

/*
7
10
0 1 200
1 2 150
0 3 100
1 4 170
1 5 180
2 5 100
3 4 240
3 6 380
4 6 210
5 6 260

930
*/