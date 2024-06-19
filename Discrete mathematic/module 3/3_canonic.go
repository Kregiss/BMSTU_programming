package main
/*
import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type MealyMachine struct {
	n     int
	m     int
	q0    int
	delta [][]int
	phi   [][]string
}

func NewMealyMachine(n, m, q0 int, delta [][]int, phi [][]string) *MealyMachine {
	return &MealyMachine{n, m, q0, delta, phi}
}

func DFS(state int, nextNumber *int, visited []bool, machine *MealyMachine, canonical []int) {
	visited[state] = true
	canonical[state] = *nextNumber
	*nextNumber++

	type Transition struct {
		NextState int
		Signal    int
	}
	var transitionsList []Transition
	for signal := range machine.delta[state] {
		transitionsList = append(transitionsList, Transition{
			NextState: machine.delta[state][signal],
			Signal:    signal,
		})
	}

	for i := range transitionsList {
		for j := i + 1; j < len(transitionsList); j++ {
			if transitionsList[i].Signal > transitionsList[j].Signal {
				transitionsList[i], transitionsList[j] = transitionsList[j], transitionsList[i]
			}
		}
	}

	for _, t := range transitionsList {
		if !visited[t.NextState] {
			DFS(t.NextState, nextNumber, visited, machine, canonical)
		}
	}
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	scanner.Scan()
	n, _ := strconv.Atoi(scanner.Text())
	scanner.Scan()
	m, _ := strconv.Atoi(scanner.Text())
	scanner.Scan()
	q0, _ := strconv.Atoi(scanner.Text())

	delta := make([][]int, n)
	phi := make([][]string, n)
	for i := 0; i < n; i++ {
		delta[i] = make([]int, m)
		scanner.Scan()
		row := strings.Split(scanner.Text(), " ")
		for j := 0; j < m; j++ {
			delta[i][j], _ = strconv.Atoi(row[j])
		}
	}
	for i := 0; i < n; i++ {
		phi[i] = make([]string, m)
		scanner.Scan()
		row := strings.Split(scanner.Text(), " ")
		for j := 0; j < m; j++ {
			phi[i][j] = row[j]
		}
	}

	machine := NewMealyMachine(n, m, q0, delta, phi)

	visited := make([]bool, n)
	canonical := make([]int, n)
	nextNumber := 0
	DFS(machine.q0, &nextNumber, visited, machine, canonical)

	newDelta := make([][]int, n)
	newPhi := make([][]string, n)
	for i := 0; i < n; i++ {
		newDelta[i] = make([]int, m)
		newPhi[i] = make([]string, m)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			newDelta[canonical[i]][j] = canonical[delta[i][j]]
			newPhi[canonical[i]][j] = phi[i][j]
		}
	}

	fmt.Println(n)
	fmt.Println(m)
	fmt.Println(0)
	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Print(newDelta[i][j], " ")
		}
		fmt.Println()
	}
	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			fmt.Print(newPhi[i][j], " ")
		}
		fmt.Println()
	}
}
*/

/*
4
3
0
1 3 3
1 1 2
2 2 2
1 2 3
x y y
y y x
x x x
x y y

Ожидаемый вывод:
4
3
0
1 3 3
1 1 2
2 2 2
1 2 3
x y y
y y x
x x x
x y y

7
3
2
1 2 1
6 4 2
3 5 6
1 1 0
5 6 5
0 3 2
6 0 1
d z l
g l l
t n h
v h n
o l l
d i v
v i d

Ожидаемый вывод:
7
3
0
1 6 3
2 2 4
3 5 0
3 4 2
2 0 2
6 3 6
4 1 0
t n h
v h n
g l l
v i d
d z l
o l l
d i v


*/
