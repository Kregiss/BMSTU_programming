package main
/*
import (
	"fmt"
)

type Graph struct {
	Vertices int
	Edges    int
	Adj      map[int][]int
}

func (g *Graph) addEdge(u, v int) {
	g.Adj[u] = append(g.Adj[u], v)
	g.Adj[v] = append(g.Adj[v], u)
	g.Edges++
}

func (g *Graph) findBridges() int {
	visited := make([]bool, g.Vertices)
	discovery := make([]int, g.Vertices)
	low := make([]int, g.Vertices)
	time := 0
	bridges := 0

	var dfs func(node, parent int)
	dfs = func(node, parent int) {
		visited[node] = true
		discovery[node] = time
		low[node] = time
		time++

		for _, neighbor := range g.Adj[node] {
			if !visited[neighbor] {
				dfs(neighbor, node)
				low[node] = min(low[node], low[neighbor])
				if low[neighbor] > discovery[node] {
					bridges++
				}
			} else if neighbor != parent {
				low[node] = min(low[node], discovery[neighbor])
			}
		}
	}

	for i := 0; i < g.Vertices; i++ {
		if !visited[i] {
			dfs(i, -1)
		}
	}

	return bridges
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func main10() {
	var N, M, u, v int
	fmt.Scan(&N, &M)

	graph := Graph{Vertices: N, Adj: make(map[int][]int)}

	for i := 0; i < M; i++ {
		fmt.Scan(&u, &v)
		graph.addEdge(u, v)
	}

	bridges := graph.findBridges()
	fmt.Println(bridges)
}


7
7
0 1
0 4
1 2
2 3
2 5
3 6
4 5

*/