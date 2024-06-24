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

	subsequences := make(chan chan int, nitems)
	for i := 0; i < nitems; i++ {
		subseq := make(chan int)
		go func(idx int) {
			defer close(subseq)
			subseq <- idx
		}(i)
		subsequences <- subseq
	}
	close(subsequences)

	for subseqCount := nitems; subseqCount > 1; subseqCount = (subseqCount + 1) / 2 {
		nextSubsequences := make(chan chan int, (subseqCount + 1) / 2)

		for i := 0; i < subseqCount / 2; i++ {
			left := <-subsequences
			right := <-subsequences
			merged := make(chan int)
			go merge(left, right, compare, merged)
			nextSubsequences <- merged
		}

		if subseqCount % 2 != 0 {
			nextSubsequences <- <-subsequences
		}

		close(nextSubsequences)
		subsequences = nextSubsequences
	}

	for idx := range <-subsequences {
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

	fmt.Print("Отсорированные индексы: ")
	for _, idx := range sortedIndices {
		fmt.Print(idx, " ")
	}
	fmt.Println()

	fmt.Print("Отсортиованные слова: ")
	for _, idx := range sortedIndices {
		fmt.Print(words[idx], " ")
	}
	fmt.Println()
}

/*
fox the dog over lazy quick brown jumps
*/
