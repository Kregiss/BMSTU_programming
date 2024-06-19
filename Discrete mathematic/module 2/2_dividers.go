package main

import (
	"fmt"
)

func divisorsGraphDot(x uint32) string {
	dotString := "graph {\n"

	var divisors []uint32

	for i := uint32(1); i <= x; i++ {
		if x % i == 0 {
			divisors = append(divisors, i)
		}
	}

	for _, div := range divisors {
		dotString += fmt.Sprintf("  %d\n", div)
	}

	for _, u := range divisors {
		for _, v := range divisors {
			if u % v == 0 && u != v {
				isMaxDivisor := true
				for _, w := range divisors {
					if u % w == 0 && w % v == 0 && u != w && v != w {
						isMaxDivisor = false
						break
					}
				}
				if isMaxDivisor {
					dotString += fmt.Sprintf("  %d--%d\n", u, v)
				}
			}
		}
	}

	dotString += "}"

	return dotString
}

func main6() {
	var x uint32
	fmt.Scan(&x)

	fmt.Println(divisorsGraphDot(x))
}

/*
4294967296


18

graph {
	18--9
	18--6
	6--3
	9--3
	6--2
	2--1
	3--1
}
*/
