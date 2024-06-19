package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Task struct {
	name     string
	duration int
}

type Graph struct {
	tasks map[string]*Task
	edges map[*Task][]*Task
}

func NewGraph() *Graph {
	return &Graph{
		tasks: make(map[string]*Task),
		edges: make(map[*Task][]*Task),
	}
}

func (g *Graph) AddTask(name string, duration int) *Task {
	if task, exists := g.tasks[name]; exists {
		return task
	}
	task := &Task{name: name, duration: duration}
	g.tasks[name] = task
	g.edges[task] = []*Task{}
	return task
}

func (g *Graph) AddEdge(from, to *Task) {
	g.edges[from] = append(g.edges[from], to)
}

func parseInput(input string) (*Graph, error) {
	graph := NewGraph()
	edges := strings.Split(input, ";")
	for _, edge := range edges {
		tasks := strings.Split(edge, "<")
		var prevTask *Task
		for _, taskStr := range tasks {
			taskStr = strings.TrimSpace(taskStr)
			openParenIdx := strings.Index(taskStr, "(")
			closeParenIdx := strings.Index(taskStr, ")")
			var task *Task
			if openParenIdx != -1 && closeParenIdx != -1 {
				name := taskStr[:openParenIdx]
				duration, _ := strconv.Atoi(taskStr[openParenIdx+1 : closeParenIdx])
				task = graph.AddTask(name, duration)
			} else {
				name := taskStr
				task = graph.tasks[name]
			}
			if prevTask != nil {
				graph.AddEdge(prevTask, task)
			}
			prevTask = task
		}
	}
	return graph, nil
}

func findCriticalPaths(graph *Graph, order []*Task, cycles map[*Task]bool) (int, map[*Task]int, map[*Task][]*Task) {
	earliestStart := make(map[*Task]int)
	prev := make(map[*Task][]*Task)

	for _, task := range order {
		for _, neighbor := range graph.edges[task] {
			if earliestStart[task]+task.duration > earliestStart[neighbor] {
				earliestStart[neighbor] = earliestStart[task] + task.duration
				prev[neighbor] = []*Task{task}
			} else if earliestStart[task]+task.duration == earliestStart[neighbor] {
				prev[neighbor] = append(prev[neighbor], task)
			}
		}
	}

	maxDuration := 0
	for task, start := range earliestStart {
		if cycles[task] {
			if start > maxDuration {
				maxDuration = start
			}
		} else {
			if start+task.duration > maxDuration {
				maxDuration = start + task.duration
			}
		}
	}

	return maxDuration, earliestStart, prev
}

func getCriticalPaths(task *Task, prev map[*Task][]*Task) [][]*Task {
	if len(prev[task]) == 0 {
		return [][]*Task{{task}}
	}
	var paths [][]*Task
	for _, predecessor := range prev[task] {
		for _, path := range getCriticalPaths(predecessor, prev) {
			paths = append(paths, append(path, task))
		}
	}
	return paths
}

func markDependentNodes(graph *Graph, cycles map[*Task]bool) {
	visited := make(map[*Task]bool)

	var mark func(*Task)
	mark = func(task *Task) {
		if visited[task] {
			return
		}
		visited[task] = true
		cycles[task] = true
		for _, neighbor := range graph.edges[task] {
			mark(neighbor)
			//fmt.Println(neighbor)
		}
	}

	for task := range cycles {
		mark(task)
	}
}

func topologicalSort(graph *Graph, cycles map[*Task]bool) []*Task {
	inDegree := make(map[*Task]int)
	for _, neighbors := range graph.edges {
		for _, neighbor := range neighbors {
			if !cycles[neighbor] {
				inDegree[neighbor]++
			}
		}
	}

	var zeroInDegree []*Task
	for _, task := range graph.tasks {
		if !cycles[task] && inDegree[task] == 0 {
			zeroInDegree = append(zeroInDegree, task)
		}
	}

	var order []*Task
	for len(zeroInDegree) > 0 {
		task := zeroInDegree[len(zeroInDegree)-1]
		zeroInDegree = zeroInDegree[:len(zeroInDegree)-1]
		order = append(order, task)
		for _, neighbor := range graph.edges[task] {
			if cycles[neighbor] {
				continue
			}
			inDegree[neighbor]--
			if inDegree[neighbor] == 0 {
				zeroInDegree = append(zeroInDegree, neighbor)
			}
		}
	}
	return order
}

func generateDot(graph *Graph, cycles map[*Task]bool, criticalEdges map[[2]*Task]bool, criticalVertices map[*Task]bool) string {
	var sb strings.Builder
	sb.WriteString("digraph {\n")

	vertexNames := make([]string, 0, len(graph.tasks))
	for vertex := range graph.tasks {
		vertexNames = append(vertexNames, vertex)
	}
	sort.Strings(vertexNames)

	addedVertices := make(map[string]bool)
	addedEdges := make(map[[2]string]bool)

	for _, vertexName := range vertexNames {
		task := graph.tasks[vertexName]
		fromName := task.name
		color := "black"
		if cycles[task] {
			color = "blue"
		} else if criticalVertices[task] {
			color = "red"
		}

		if !addedVertices[fromName] {
			if color == "black" {
				sb.WriteString(fmt.Sprintf("\t%s [label = \"%s(%d)\"]\n", fromName, fromName, task.duration))
			} else {
				sb.WriteString(fmt.Sprintf("\t%s [label = \"%s(%d)\", color = %s]\n", fromName, fromName, task.duration, color))
			}
			addedVertices[fromName] = true
		}
	}

	for _, vertexName := range vertexNames {
		task := graph.tasks[vertexName]
		fromName := task.name

		neighborsNames := make([]string, len(graph.edges[task]))
		for i, neighbor := range graph.edges[task] {
			neighborsNames[i] = neighbor.name
		}
		sort.Strings(neighborsNames)

		for _, neighborName := range neighborsNames {
			neighbor := graph.tasks[neighborName]
			toName := neighbor.name

			edge := [2]string{fromName, toName}
			edgePair := [2]*Task{task, neighbor}
			if !addedEdges[edge] {
				edgeColor := "black"
				if cycles[task] && cycles[neighbor] {
					edgeColor = "blue"
				} else if criticalEdges[edgePair] {
					edgeColor = "red"
				}
				if edgeColor == "black" {
					sb.WriteString(fmt.Sprintf("\t%s -> %s\n", fromName, toName))
				} else {
					sb.WriteString(fmt.Sprintf("\t%s -> %s [color = %s]\n", fromName, toName, edgeColor))
				}
				addedEdges[edge] = true
			}
		}
	}

	sb.WriteString("}\n")
	return sb.String()
}

func findCyclesUsingTarjan(graph *Graph) map[*Task]bool {
	index := 0
	stack := []*Task{}
	onStack := make(map[*Task]bool)
	indices := make(map[*Task]int)
	lowLink := make(map[*Task]int)
	cycles := make(map[*Task]bool)

	var tarjan func(*Task)
	tarjan = func(v *Task) {
		indices[v] = index
		lowLink[v] = index
		index++
		stack = append(stack, v)
		onStack[v] = true

		for _, w := range graph.edges[v] {
			if _, found := indices[w]; !found {
				tarjan(w)
				lowLink[v] = min(lowLink[v], lowLink[w])
			} else if onStack[w] {
				lowLink[v] = min(lowLink[v], indices[w])
			}
		}

		if lowLink[v] == indices[v] {
			var scc []*Task
			for {
				w := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				onStack[w] = false
				scc = append(scc, w)
				if w == v {
					break
				}
			}
			if len(scc) > 1 {
				for _, u := range scc {
					cycles[u] = true
				}
			} else if len(scc) == 1 && graph.edges[scc[0]] != nil && contains(graph.edges[scc[0]], scc[0]) {
				cycles[scc[0]] = true
			}
		}
	}

	for _, v := range graph.tasks {
		if _, found := indices[v]; !found {
			tarjan(v)
		}
	}

	return cycles
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func contains(tasks []*Task, task *Task) bool {
	for _, t := range tasks {
		if t == task {
			return true
		}
	}
	return false
}

func hasIncomingEdges(graph *Graph, vertex *Task) bool {
	vertexNames := make([]string, 0, len(graph.tasks))
	for vertex := range graph.tasks {
		vertexNames = append(vertexNames, vertex)
	}
	for _, vertexName := range vertexNames {
		task := graph.tasks[vertexName]

		neighborsNames := make([]string, len(graph.edges[task]))
		for i, neighbor := range graph.edges[task] {
			neighborsNames[i] = neighbor.name
		}

		for _, neighborName := range neighborsNames {
			neighbor := graph.tasks[neighborName]
			toName := neighbor.name
			if toName == vertex.name {
				//fmt.Println("to", toName)
				return true
			}
		}
	}
	return false
}

func findMaxDurationNonCriticalVertices(graph *Graph, cycles map[*Task]bool) map[*Task]bool {
	maxDuration := 0
	for _, task := range graph.tasks {
		if (len(graph.edges[task]) == 0 || !hasIncomingEdges(graph, task)) && !cycles[task] {
			if task.duration > maxDuration {
				maxDuration = task.duration
			}
		}
	}

	maxDurationNonCriticalVertices := make(map[*Task]bool)
	for _, task := range graph.tasks {
		if (len(graph.edges[task]) == 0 || !hasIncomingEdges(graph, task)) && task.duration == maxDuration && !cycles[task] {
			maxDurationNonCriticalVertices[task] = true
		}
	}
	return maxDurationNonCriticalVertices
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
		if !strings.HasSuffix(line, ";") && !strings.HasSuffix(line, "<") {
			break
		}
	}

	input := strings.Join(lines, "")
	input = strings.ReplaceAll(input, "\n", "")
	input = strings.ReplaceAll(input, "\t", "")
	input = strings.ReplaceAll(input, " <", "<")
	input = strings.ReplaceAll(input, "< ", "<")
	input = strings.ReplaceAll(input, ";\n", ";")

	graph, _ := parseInput(input)
	cycles := findCyclesUsingTarjan(graph)
	markDependentNodes(graph, cycles)
	order := topologicalSort(graph, cycles)

	maxDuration, earliestStart, prev := findCriticalPaths(graph, order, cycles)

	criticalVertices := make(map[*Task]bool)
	criticalEdges := make(map[[2]*Task]bool)

	for task := range graph.tasks {
		taskObj := graph.tasks[task]
		if earliestStart[taskObj]+taskObj.duration == maxDuration {
			paths := getCriticalPaths(taskObj, prev)
			for _, path := range paths {
				for i := 0; i < len(path)-1; i++ {
					criticalEdges[[2]*Task{path[i], path[i+1]}] = true
					criticalVertices[path[i]] = true
					criticalVertices[path[i+1]] = true
				}
			}
		}
	}

	if len(criticalVertices) == 0 {
		nonCriticalVertices := findMaxDurationNonCriticalVertices(graph, cycles)
		if len(nonCriticalVertices) > 0 {
			criticalVertices = nonCriticalVertices
		}
	}

	dot := generateDot(graph, cycles, criticalEdges, criticalVertices)
	fmt.Print(dot)
}


/*
A(5) < B(5) < C(10);
C < B

digraph {
  A [label = "A(5)", color = red]
  B [label = "B(5)", color = blue]
  C [label = "C(10)", color = blue]
  A -> B
  B -> C [color = blue]
  C -> B [color = blue]
}

A(5);
B(5)

digraph {
  A [label = "A(5)", color = red]
  B [label = "B(5)", color = red]
}

A(5) < B(5) < C(10) < D(15) < E(5) < F(10);
D < C

digraph {
	A [label = "A(5)", color = red]
	B [label = "B(5)", color = red]
	C [label = "C(10)", color = blue]
	D [label = "D(15)", color = blue]
	E [label = "E(5)", color = blue]
	F [label = "F(10)", color = blue]
	A -> B [color = red]
	B -> C
	C -> D [color = blue]
	D -> C [color = blue]
	D -> E [color = blue]
	E -> F [color = blue]
}

A(15) < B(15) < C(15);
D(5) < E(15) < F(25);
H(5) < I(5) < J(5)

digraph {
	A [label = "A(15)", color = red]
	B [label = "B(15)", color = red]
	C [label = "C(15)", color = red]
	D [label = "D(5)", color = red]
	E [label = "E(15)", color = red]
	F [label = "F(25)", color = red]
	H [label = "H(5)"]
	I [label = "I(5)"]
	J [label = "J(5)"]
	A -> B [color = red]
	B -> C [color = red]
	D -> E [color = red]
	E -> F [color = red]
	H -> I
	I -> J
}

BuyFood(30) <
    CutMeat(5) <
    BoilMeat(60) <
    BoilAll(30) <
    ServeUp(1);
BuyFood <
    PeelPotatoes(5) <
    CutPotatoes(5) <
    BoilAll;
BuyFood <
    CutCabbage(10) <
    BoilAll;
BuyFood <
    CutOnion(3) <
    FryOnion(8) <
    ServeUp

digraph {
	BoilAll [label = "BoilAll(30)", color = red]
	BoilMeat [label = "BoilMeat(60)", color = red]
	BuyFood [label = "BuyFood(30)", color = red]
	CutCabbage [label = "CutCabbage(10)"]
	CutMeat [label = "CutMeat(5)", color = red]
	CutOnion [label = "CutOnion(3)"]
	CutPotatoes [label = "CutPotatoes(5)"]
	FryOnion [label = "FryOnion(8)"]
	PeelPotatoes [label = "PeelPotatoes(5)"]
	ServeUp [label = "ServeUp(1)", color = red]
	BoilAll -> ServeUp [color = red]
	BoilMeat -> BoilAll [color = red]
	BuyFood -> CutCabbage
	BuyFood -> CutMeat [color = red]
	BuyFood -> CutOnion
	BuyFood -> PeelPotatoes
	CutCabbage -> BoilAll
	CutMeat -> BoilMeat [color = red]
	CutOnion -> FryOnion
	CutPotatoes -> BoilAll
	FryOnion -> ServeUp
	PeelPotatoes -> CutPotatoes
}

Think(10) < Work(8) < Rest(2);
Work < Win(100);
Rest < Work;
Think < Eat(2) < Drink(3);
Think < Mourn(24)

digraph {
	Drink [label = "Drink(3)"]
	Eat [label = "Eat(2)"]
	Mourn [label = "Mourn(24)", color = red]
	Rest [label = "Rest(2)", color = blue]
	Think [label = "Think(10)", color = red]
	Win [label = "Win(100)", color = blue]
	Work [label = "Work(8)", color = blue]
	Eat -> Drink
	Rest -> Work [color = blue]
	Think -> Eat
	Think -> Mourn [color = red]
	Think -> Work
	Work -> Rest [color = blue]
	Work -> Win [color = blue]
}
*/
