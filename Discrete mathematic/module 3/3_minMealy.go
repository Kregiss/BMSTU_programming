package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type State struct {
	id          int
	transitions []int
	outputs     []string
}

func Find(parent []int, x int) int {
	if parent[x] != x {
		parent[x] = Find(parent, parent[x])
	}
	return parent[x]
}

func Union(parent, rank []int, x, y int) {
	rootX := Find(parent, x)
	rootY := Find(parent, y)
	if rootX != rootY {
		if rank[rootX] > rank[rootY] {
			parent[rootY] = rootX
		} else if rank[rootX] < rank[rootY] {
			parent[rootX] = rootY
		} else {
			parent[rootY] = rootX
			rank[rootX]++
		}
	}
}

func Split1(n int, states []State, m int) (int, []int) {
	parent := make([]int, n)
	rank := make([]int, n)
	for i := 0; i < n; i++ {
		parent[i] = i
		rank[i] = 0
	}

	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			eq := true
			for k := 0; k < m; k++ {
				if states[i].outputs[k] != states[j].outputs[k] {
					eq = false
					break
				}
			}
			if eq {
				Union(parent, rank, i, j)
			}
		}
	}

	classCount := 0
	classMap := make(map[int]int)
	classes := make([]int, n)
	for i := 0; i < n; i++ {
		root := Find(parent, i)
		if _, exists := classMap[root]; !exists {
			classMap[root] = classCount
			classCount++
		}
		classes[i] = classMap[root]
	}
	return classCount, classes
}

func Split(n int, states []State, m int, classes []int) (int, []int) {
	parent := make([]int, n)
	rank := make([]int, n)
	for i := 0; i < n; i++ {
		parent[i] = i
		rank[i] = 0
	}

	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			if classes[i] == classes[j] {
				eq := true
				for k := 0; k < m; k++ {
					if classes[states[i].transitions[k]] != classes[states[j].transitions[k]] {
						eq = false
						break
					}
				}
				if eq {
					Union(parent, rank, i, j)
				}
			}
		}
	}

	classCount := 0
	classMap := make(map[int]int)
	newClasses := make([]int, n)
	for i := 0; i < n; i++ {
		root := Find(parent, i)
		if _, exists := classMap[root]; !exists {
			classMap[root] = classCount
			classCount++
		}
		newClasses[i] = classMap[root]
	}
	return classCount, newClasses
}

func AufenkampHohn(n, m, q0 int, states []State) ([]State, int) {
	classCount, classes := Split1(n, states, m)

	for {
		newClassCount, newClasses := Split(n, states, m, classes)
		if newClassCount == classCount {
			break
		}
		classCount = newClassCount
		classes = newClasses
	}

	minimizedStates := make([]State, classCount)
	for i := 0; i < classCount; i++ {
		minimizedStates[i] = State{
			id:          i,
			transitions: make([]int, m),
			outputs:     make([]string, m),
		}
	}

	for i := 0; i < n; i++ {
		newState := classes[i]
		for j := 0; j < m; j++ {
			minimizedStates[newState].transitions[j] = classes[states[i].transitions[j]]
			minimizedStates[newState].outputs[j] = states[i].outputs[j]
		}
	}

	initialState := classes[q0]

	visited := make([]bool, classCount)
	currentID := 0
	stateMapping := make(map[int]int)

	var dfs func(int)
	dfs = func(state int) {
		if visited[state] {
			return
		}
		visited[state] = true
		stateMapping[state] = currentID
		currentID++
		for _, next := range minimizedStates[state].transitions {
			dfs(next)
		}
	}

	dfs(initialState)

	newMinimizedStates := make([]State, classCount)
	for oldID, newID := range stateMapping {
		newMinimizedStates[newID] = minimizedStates[oldID]
		newMinimizedStates[newID].id = newID
	}

	for i := range newMinimizedStates {
		for j := range newMinimizedStates[i].transitions {
			newMinimizedStates[i].transitions[j] = stateMapping[newMinimizedStates[i].transitions[j]]
		}
	}

	return newMinimizedStates, stateMapping[initialState]
}

func GenerateDOT(states []State, initialState int, m int, alphabet []rune) {
	fmt.Println("digraph {")
	fmt.Println("    rankdir = LR")
	for _, state := range states {
		for i := 0; i < m; i++ {
			fmt.Printf("    %d -> %d [label = \"%c(%s)\"]\n", state.id, state.transitions[i], alphabet[i], state.outputs[i])
		}
	}
	fmt.Println("}")
}

func main() {
	reader := bufio.NewReader(os.Stdin)

	line, _ := reader.ReadString('\n')
	n, _ := strconv.Atoi(strings.TrimSpace(line))

	line, _ = reader.ReadString('\n')
	m, _ := strconv.Atoi(strings.TrimSpace(line))

	line, _ = reader.ReadString('\n')
	q0, _ := strconv.Atoi(strings.TrimSpace(line))

	states := make([]State, n)
	for i := 0; i < n; i++ {
		states[i] = State{
			id:          i,
			transitions: make([]int, m),
			outputs:     make([]string, m),
		}
	}

	for i := 0; i < n; i++ {
		line, _ = reader.ReadString('\n')
		line = strings.TrimSpace(line)
		parts := strings.Split(line, " ")
		for j := 0; j < m; j++ {
			states[i].transitions[j], _ = strconv.Atoi(parts[j])
		}
	}

	for i := 0; i < n; i++ {
		line, _ = reader.ReadString('\n')
		line = strings.TrimSpace(line)
		parts := strings.Split(line, " ")
		for j := 0; j < m; j++ {
			states[i].outputs[j] = parts[j]
		}
	}

	alphabet := []rune("abcdefghijklmnopqrstuvwxyz")

	minimizedStates, initialState := AufenkampHohn(n, m, q0, states)

	GenerateDOT(minimizedStates, initialState, m, alphabet)
}

/*
5
3
0
1 2 3
3 4 1
3 4 2
3 0 4
4 4 3
x x y
y x x
y x x
x x y
x y x

digraph {
    rankdir = LR
    0 -> 1 [label = "a(x)"]
    0 -> 1 [label = "b(x)"]
    0 -> 2 [label = "c(y)"]
    1 -> 2 [label = "a(y)"]
    1 -> 3 [label = "b(x)"]
    1 -> 1 [label = "c(x)"]
    2 -> 2 [label = "a(x)"]
    2 -> 0 [label = "b(x)"]
    2 -> 3 [label = "c(y)"]
    3 -> 3 [label = "a(x)"]
    3 -> 3 [label = "b(y)"]
    3 -> 2 [label = "c(x)"]
}

7
3
1
6 3 5
6 6 6
6 6 6
3 6 6
3 5 2
3 5 1
0 2 5
p n q
s v p
s v p
q f n
j h q
j h q
j r u

digraph {
    rankdir = LR
    0 -> 1 [label = "a(s)"]
    0 -> 1 [label = "b(v)"]
    0 -> 1 [label = "c(p)"]
    1 -> 2 [label = "a(j)"]
    1 -> 0 [label = "b(r)"]
    1 -> 4 [label = "c(u)"]
    2 -> 1 [label = "a(p)"]
    2 -> 3 [label = "b(n)"]
    2 -> 4 [label = "c(q)"]
    3 -> 3 [label = "a(q)"]
    3 -> 1 [label = "b(f)"]
    3 -> 1 [label = "c(n)"]
    4 -> 3 [label = "a(j)"]
    4 -> 4 [label = "b(h)"]
    4 -> 0 [label = "c(q)"]
}
*/
