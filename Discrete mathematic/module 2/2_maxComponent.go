package main
/*
import (
	"fmt"
)

// Вершина
type Vertex struct {
	number int
	edges  []*Edge
}

// Ребро
type Edge struct {
	start *Vertex
	end   *Vertex
}

func NewVertex(number int) *Vertex {
	return &Vertex{number: number}
}

func NewEdge(start, end *Vertex) *Edge {
	return &Edge{start: start, end: end}
}

func LargestConnectedComponent(N int, edges [][]int) ([]*Vertex, []*Vertex) {
	vertices := make([]*Vertex, N)

	for i := 0; i < N; i++ {
		vertices[i] = NewVertex(i)
	}

	for _, e := range edges {
		u, v := e[0], e[1]

		vertices[u].edges = append(vertices[u].edges, NewEdge(vertices[u], vertices[v]))
		vertices[v].edges = append(vertices[v].edges, NewEdge(vertices[v], vertices[u]))
	}

	visited := make([]bool, N)
	largestComponentSize := 0
	var largestComponentVertices []*Vertex

	for _, vertex := range vertices {
		if !visited[vertex.number] {
			var componentVertices []*Vertex
			stack := []*Vertex{vertex}
			for len(stack) > 0 {
				currentVertex := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				if !visited[currentVertex.number] {
					visited[currentVertex.number] = true
					componentVertices = append(componentVertices, currentVertex)
					for _, edge := range currentVertex.edges {
						stack = append(stack, edge.end)
					}
				}
			}
			if len(componentVertices) > largestComponentSize {
				largestComponentSize = len(componentVertices)
				largestComponentVertices = componentVertices
			}
		}
	}

	return largestComponentVertices, vertices
}

func main9() {
	var N, M int
	fmt.Scan(&N, &M)

	edges := make([][]int, M)
	for i := 0; i < M; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		edges[i] = []int{u, v}
	}

	largestComponentVertices, vertices := LargestConnectedComponent(N, edges)

	largestComponentSet := make(map[int]bool)
	for _, vertex := range largestComponentVertices {
		largestComponentSet[vertex.number] = true
	}

	fmt.Println("graph {")
	for _, x := range vertices {
		isXinLargestComponent := false
		for _, vert := range largestComponentVertices {
			if vert == x {
				isXinLargestComponent = true
				break
			}
		}

		if isXinLargestComponent {
			fmt.Printf("  %d [color=red]\n", x.number)
		} else {
			fmt.Printf("  %d\n", x.number)
		}
	}

	for _, edge := range edges {
		u, v := edge[0], edge[1]
		fmt.Printf("  %d--%d", u, v)
		for _, vertex := range largestComponentVertices {
			if vertex.number == u || vertex.number == v {
				fmt.Print(" [color=red]")
				break
			}
		}
		fmt.Println()
	}
	fmt.Println("}")
}
*/
/*
7
8
0 1
0 5
1 5
1 4
5 4
2 3
3 6
*/