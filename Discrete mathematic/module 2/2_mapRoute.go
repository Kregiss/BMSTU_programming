package main

import (
	"container/heap"
	"fmt"
)

type Node struct {
	x, y, dist int
}

type MinHeap []Node

func (h MinHeap) Len() int            { return len(h) }
func (h MinHeap) Less(i, j int) bool  { return h[i].dist < h[j].dist }
func (h MinHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *MinHeap) Push(x interface{}) { *h = append(*h, x.(Node)) }
func (h *MinHeap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

func minPathLength(N int, grid [][]int) int {
	dx := [4]int{1, 0, -1, 0}
	dy := [4]int{0, 1, 0, -1}

	visited := make([][]bool, N)
	for i := range visited {
		visited[i] = make([]bool, N)
	}

	dist := make([][]int, N)
	for i := range dist {
		dist[i] = make([]int, N)
		for j := range dist[i] {
			dist[i][j] = -1
		}
	}

	pq := &MinHeap{}
	heap.Push(pq, Node{0, 0, grid[0][0]})
	dist[0][0] = grid[0][0]

	for pq.Len() > 0 {
		node := heap.Pop(pq).(Node)
		x, y, d := node.x, node.y, node.dist
		if x == N-1 && y == N-1 {
			return d
		}

		if visited[x][y] {
			continue
		}
		visited[x][y] = true

		for k := 0; k < 4; k++ {
			nx, ny := x+dx[k], y+dy[k]
			if nx >= 0 && nx < N && ny >= 0 && ny < N && (dist[nx][ny] == -1 || dist[nx][ny] > d+grid[nx][ny]) {
				dist[nx][ny] = d + grid[nx][ny]
				heap.Push(pq, Node{nx, ny, dist[nx][ny]})
			}
		}
	}

	return -1
}

func main() {
	var N int
	fmt.Scan(&N)

	grid := make([][]int, N)
	for i := range grid {
		grid[i] = make([]int, N)
		for j := range grid[i] {
			fmt.Scan(&grid[i][j])
		}
	}

	fmt.Println(minPathLength(N, grid))
}

/*
5
1 7 7 9 1
8 5 0 6 0
4 1 2 9 8
4 1 5 7 6
5 6 8 8 7

40
*/
