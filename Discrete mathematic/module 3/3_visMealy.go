package main

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

func (mm *MealyMachine) GenerateDOT() string {
	var sb strings.Builder
	sb.WriteString("digraph {\n")
	sb.WriteString("    rankdir = LR\n")

	alphabet := "abcdefghijklmnopqrstuvwxyz"

	for i := 0; i < mm.n; i++ {
		for j := 0; j < mm.m; j++ {
			sb.WriteString(fmt.Sprintf("    %d -> %d [label = \"%c(%s)\"]\n",
				i, mm.delta[i][j], alphabet[j], mm.phi[i][j]))
		}
	}

	sb.WriteString("}")
	return sb.String()
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
	fmt.Println(machine.GenerateDOT())
}

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

digraph {
    rankdir = LR
    0 -> 1 [label = "a(x)"]
    0 -> 3 [label = "b(y)"]
    0 -> 3 [label = "c(y)"]
    1 -> 1 [label = "a(y)"]
    1 -> 1 [label = "b(y)"]
    1 -> 2 [label = "c(x)"]
    2 -> 2 [label = "a(x)"]
    2 -> 2 [label = "b(x)"]
    2 -> 2 [label = "c(x)"]
    3 -> 1 [label = "a(x)"]
    3 -> 2 [label = "b(y)"]
    3 -> 3 [label = "c(y)"]
}
*/
