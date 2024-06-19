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

func (mm *MealyMachine) GenerateWords(M int) []string {
	type stateWord struct {
		state int
		word  string
	}

	words := make(map[string]struct{})
	visited := make(map[stateWord]bool)

	queue := []stateWord{{state: mm.q0, word: ""}}
	visited[queue[0]] = true

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if len(current.word) > 0 && len(current.word) <= M {
			words[current.word] = struct{}{}
		}

		if len(current.word) >= M {
			continue
		}

		for i := 0; i < mm.m; i++ {
			nextState := mm.delta[current.state][i]
			output := mm.phi[current.state][i]
			newWord := current.word
			if output != "-" {
				newWord += output
			}

			newStateWord := stateWord{state: nextState, word: newWord}
			if !visited[newStateWord] {
				visited[newStateWord] = true
				queue = append(queue, newStateWord)
			}
		}
	}

	result := make([]string, 0, len(words))
	for word := range words {
		result = append(result, word)
	}
	return result
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	scanner.Scan()
	n, _ := strconv.Atoi(scanner.Text())

	scanner.Scan()

	delta := make([][]int, n)
	for i := 0; i < n; i++ {
		delta[i] = make([]int, 2)
		scanner.Scan()
		row := strings.Split(scanner.Text(), " ")
		for j := 0; j < 2; j++ {
			delta[i][j], _ = strconv.Atoi(row[j])
		}
	}

	scanner.Scan()

	phi := make([][]string, n)
	for i := 0; i < n; i++ {
		phi[i] = make([]string, 2)
		scanner.Scan()
		row := strings.Split(scanner.Text(), " ")
		for j := 0; j < 2; j++ {
			phi[i][j] = row[j]
		}
	}

	scanner.Scan()

	scanner.Scan()
	q0, _ := strconv.Atoi(scanner.Text())

	scanner.Scan()
	M, _ := strconv.Atoi(scanner.Text())

	machine := NewMealyMachine(n, 2, q0, delta, phi)

	words := machine.GenerateWords(M)
	//fmt.Println("Количество слов: ", len(words))
	for _, word := range words {
		fmt.Print(word, " ")
	}
	fmt.Println()
}
*/

/*
4

0 1
0 2
1 3
3 3

- x
- y
- x
y z

0
4

Количество слов: 16
x xx xxx xxxx xxxy xxy xxyx xxyy
xy xyx xyxx xyxy xyxz xyy xyyx xyyy


33

30 10
14 6
13 19
27 1
26 0
8 13
31 21
2 11
32 14
5 1
17 29
9 25
18 4
0 2
22 23
9 8
20 3
32 29
27 23
4 30
9 17
0 1
32 16
16 24
4 28
22 29
19 15
18 30
20 30
3 6
22 7
12 20
31 8

z z
z -
- z
y x
y y
z y
z x
x -
- x
x -
z y
- z
y y
x y
x y
y y
y x
z x
- -
- z
z z
z -
y z
- y
z -
z x
- z
- x
- x
z y
- -
x x
- z

10
6

Количество слов: 140
y yy yyx yyxx yyxxx yyxxxx yyxxxz yyxxz
yyxxzx yyxxzy yyxxzz yyxz yyxzx yyxzxy yyxzxz yyxzy
yyxzyx yyxzyy yyxzz yyxzzx yyxzzy yyxzzz yyz yyzx
yyzxy yyzxyx yyzxyy yyzxz yyzxzx yyzxzz yz yzx
yzxx yzxxx yzxxxx yzxxxz yzxxz yzxxzx yzxxzy yzxxzz
yzxz yzxzx yzxzxy yzxzxz yzxzy yzxzyx yzxzyy yzy
yzyx yzyxx yzyxxx yzyxxy yzyxxz yzyxy yzyxyx yzyxyy
yzyxyz yzyxz yzyxzx yzyxzy yzyxzz yzyy yzyyx yzyyxx
yzyyxy yzyyxz yzyyz yzyyzx yzyyzy yzyyzz z zx
zxy zxyx zxyxx zxyxxx zxyxxz zxyxz zxyxzx zxyxzy
zxyxzz zxyz zxyzx zxyzxy zxyzxz zxz zxzx zxzxx
zxzxxx zxzxxz zxzxz zxzxzx zxzxzy zxzy zxzyx zxzyxx
zxzyxy zxzyxz zxzyy zxzyyx zxzyyz zz zzx zzxy
zzxyx zzxyxx zzxyxy zzxyxz zzxyy zzxyyx zzxyyy zzxyyz
zzxz zzxzx zzxzxx zzxzxy zzxzxz zzxzz zzxzzx zzxzzy
zzxzzz zzz zzzx zzzxx zzzxxy zzzxxz zzzxy zzzxyx
zzzxyy zzzxz zzzxzx zzzxzz zzzz zzzzx zzzzxx zzzzxy
zzzzxz zzzzz zzzzzx zzzzzz

20

10 11
8 5
4 19
9 3
6 12
13 17
12 2
11 13
12 12
8 7
15 0
16 8
6 4
13 4
5 9
14 0
18 4
8 16
13 1
10 3

x -
- z
z x
y y
- -
y y
z z
z x
- x
x -
x y
y x
- z
y y
x z
z y
z x
z -
z -
x -

0
15

*/
