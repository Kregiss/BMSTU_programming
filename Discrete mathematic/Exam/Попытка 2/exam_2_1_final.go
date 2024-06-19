// не синхронные

package main

import (
	"fmt"
)

func MergeSeqSort(nitems int, compare func(i, j int) int, indices chan int) {
	defer close(indices)

	if nitems <= 1 {
		if nitems == 1 {
			indices <- 0
		}
		return
	}

	subsequences := make([]chan int, nitems)
	for i := 0; i < nitems; i++ {
		subsequences[i] = make(chan int, 1)
		subsequences[i] <- i
		close(subsequences[i])
	}

	for len(subsequences) > 1 {
		nextSubsequences := []chan int{}
		for i := 0; i < len(subsequences); i += 2 {
			if i + 1 < len(subsequences) {
				merged := make(chan int)
				go merge(subsequences[i], subsequences[i+1], compare, merged)
				nextSubsequences = append(nextSubsequences, merged)
			} else {
				nextSubsequences = append(nextSubsequences, subsequences[i])
			}
		}
		subsequences = nextSubsequences
	}

	for idx := range subsequences[0] {
		indices <- idx
	}
}

func merge(left, right chan int, compare func(i, j int) int, out chan int) {
	defer close(out)
	l, lOk := <-left
	r, rOk := <-right

	for lOk && rOk {
		if compare(l, r) <= 0 {
			out <- l
			l, lOk = <-left
		} else {
			out <- r
			r, rOk = <-right
		}
	}

	for lOk {
		out <- l
		l, lOk = <-left
	}

	for rOk {
		out <- r
		r, rOk = <-right
	}
}

func main() {
	words := []string{"quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"}

	compare := func(i, j int) int {
		if len(words[i]) < len(words[j]) {
			return -1
		} else if len(words[i]) > len(words[j]) {
			return 1
		}
		return 0
	}

	indices := make(chan int)

	go MergeSeqSort(len(words), compare, indices)

	sortedIndices := []int{}
	for idx := range indices {
		sortedIndices = append(sortedIndices, idx)
	}

	fmt.Print("Отсортированные индексы: ")
	for _, idx := range sortedIndices {
		fmt.Print(idx, " ")
	}
	fmt.Println()

	fmt.Print("Отсортированные слова: ")
	for _, idx := range sortedIndices {
		fmt.Print(words[idx], " ")
	}
	fmt.Println()
}
