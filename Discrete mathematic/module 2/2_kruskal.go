package main
/*
import (
	"fmt"
	"math"
	"sort"
)

type Subset struct {
	parent []int
	rank   []int
}

func NewSubset(n int) *Subset {
	parent := make([]int, n)
	rank := make([]int, n)
	for i := range parent {
		parent[i] = i
	}
	return &Subset{parent, rank}
}

func (dsu *Subset) Find(x int) int {
	if dsu.parent[x] != x {
		dsu.parent[x] = dsu.Find(dsu.parent[x])
	}
	return dsu.parent[x]
}

func (dsu *Subset) Union(x, y int) bool {
	rootX := dsu.Find(x)
	rootY := dsu.Find(y)
	if rootX == rootY {
		return false
	}
	if dsu.rank[rootX] < dsu.rank[rootY] {
		dsu.parent[rootX] = rootY
	} else if dsu.rank[rootX] > dsu.rank[rootY] {
		dsu.parent[rootY] = rootX
	} else {
		dsu.parent[rootY] = rootX
		dsu.rank[rootX]++
	}
	return true
}

type Edge struct {
	u, v   int
	weight float64
}

type ByWeight []Edge

func (a ByWeight) Len() int           { return len(a) }
func (a ByWeight) Less(i, j int) bool { return a[i].weight < a[j].weight }
func (a ByWeight) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }

func kruskalMinSpanningTree(points [][]int) []Edge {
	n := len(points)
	edges := make([]Edge, 0)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			distance := math.Sqrt(math.Pow(float64(points[i][0] - points[j][0]), 2) + math.Pow(float64(points[i][1] - points[j][1]), 2))
			edges = append(edges, Edge{i, j, distance})
		}
	}
	sort.Sort(ByWeight(edges))

	mstEdges := make([]Edge, 0)
	dsu := NewSubset(n)
	for _, edge := range edges {
		if dsu.Union(edge.u, edge.v) {
			mstEdges = append(mstEdges, edge)
		}
	}

	return mstEdges
}

func main() {
	var n int
	fmt.Scan(&n)

	points := make([][]int, n)
	for i := 0; i < n; i++ {
		var x, y int
		fmt.Scan(&x, &y)
		points[i] = []int{x, y}
	}

	mstEdges := kruskalMinSpanningTree(points)
	totalDistance := 0.0
	for _, edge := range mstEdges {
		u, v := edge.u, edge.v
		totalDistance += math.Sqrt(math.Pow(float64(points[u][0] - points[v][0]), 2) + math.Pow(float64(points[u][1] - points[v][1]), 2))
	}
	fmt.Printf("%.2f", totalDistance)
}
*/
/*
12
2 4
2 5
3 4
3 5
6 5
6 6
7 5
7 6
5 1
5 2
6 1
6 2

14.83
*/