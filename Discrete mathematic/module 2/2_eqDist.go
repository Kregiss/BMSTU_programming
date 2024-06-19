package main

import (
	"fmt"
	"sort"
)

type Graph struct {
	Vertices int
	Edges    [][]int
}

func NewGraph(vertices int) *Graph {
	return &Graph{
		Vertices: vertices,
		Edges:    make([][]int, vertices),
	}
}

func (g *Graph) AddEdge(u, v int) {
	g.Edges[u] = append(g.Edges[u], v)
	g.Edges[v] = append(g.Edges[v], u) 
}

func (g *Graph) BFS(start int) map[int]int {
	distances := make(map[int]int)
	queue := []int{start}
	visited := make(map[int]bool)
	visited[start] = true

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]

		for _, neighbor := range g.Edges[node] {
			if !visited[neighbor] {
				visited[neighbor] = true
				queue = append(queue, neighbor)
				distances[neighbor] = distances[node] + 1
			}
		}
	}

	//fmt.Println(distances)

	return distances
}

func findEquidistantVertices(graph *Graph, anchors []int, K int) []int {
	distances := make([]map[int]int, K)

	for i, anchor := range anchors {
		distances[i] = graph.BFS(anchor)
	}

	equidistantVertices := make([]int, 0)
	for vertex := range distances[0] {
		equidistant := true
		for i := 1; i < K; i++ {
			if _, ok := distances[i][vertex]; !ok || distances[i][vertex] != distances[0][vertex] {
				equidistant = false
				break
			}
		}
		if equidistant {
			equidistantVertices = append(equidistantVertices, vertex)
		}
	}
	return equidistantVertices
}

func main() {
	var N, M, K int
	fmt.Scan(&N)
	fmt.Scan(&M)

	graph := NewGraph(N)
	for i := 0; i < M; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		graph.AddEdge(u, v)
	}

	fmt.Scan(&K)

	anchors := make([]int, K)
	for i := 0; i < K; i++ {
		fmt.Scan(&anchors[i])
	}

	result := findEquidistantVertices(graph, anchors, K)
	if len(result) == 0 {
		fmt.Println("минус")
	} else {
		sort.Ints(result)
		for _, x := range result {
			fmt.Print(x, " ")
		}
	}
}



/*
10
10
0 1
8 4
7 0
1 7
8 3
3 9
7 2
3 6
3 5
7 8
2
3 4

0 1 2 7 8


10
20
6 1
5 4
5 7
8 0
9 5
0 2
6 7
9 8
0 5
5 2
7 4
9 0
6 5
9 3
0 3
2 9
2 6
4 8
6 0
6 9
2
8 2

0 3 7 9
*/
