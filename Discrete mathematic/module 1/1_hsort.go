package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func hsort(n int, less func(i, j int) bool, swap func(i, j int)) {
    heapify := func(i, length int) {
        for {
            l := 2 * i + 1
            r := l + 1
            j := i

            if l < length && less(j, l) {
                j = l
            }
            if r < length && less(j, r) {
                j = r
            }
            if j == i {
                break
            }
            swap(i, j)
            i = j
        }
    }

    for i := n/2 - 1; i >= 0; i-- {
        heapify(i, n)
    }

    for i := n - 1; i > 0; i-- {
        swap(0, i)
        heapify(0, i)
    }
}

func main0() {
    reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')
	input = strings.TrimSpace(input)
	values := strings.Fields(input)

	var data []int
	for _, val := range values {
		num, _ := strconv.Atoi(val)
		data = append(data, num)
	}
    
    less := func(i, j int) bool {
        return data[i] < data[j]
    }
    
    swap := func(i, j int) {
        data[i], data[j] = data[j], data[i]
    }
    
    hsort(len(data), less, swap)
    
    for _, num := range data {
        fmt.Print(num, " ")
    }
}