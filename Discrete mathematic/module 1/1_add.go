package main

import (
	"fmt"
	"strconv"
)

// [5, 2, 9] = 5 + 17*2 + 17*17*9
// [4, 6, 8] = 4 + 17*6 + 17*17*8
func add(a, b []int32, p int) []int32 {
	maxLength := len(a)
	if len(b) > maxLength {
		maxLength = len(b)
	}

	allSumList := make([]int32, maxLength+1)

	carry := int32(0)

	for i := 0; i < maxLength; i++ {
		nowSum := carry
		if i < len(a) {
			nowSum += a[i]
		}
		if i < len(b) {
			nowSum += b[i]
		}

		allSumList[i] = nowSum % int32(p)
		carry = nowSum / int32(p)
	}

	if carry > 0 {
		allSumList[maxLength] = carry
	}

	var isZeroAtEnd bool = false
	//fmt.Print(allSumList[0], allSumList[maxLength])
	if allSumList[maxLength] == int32(0) {
		isZeroAtEnd = true
	}

	//fmt.Print(allSumList)
	if isZeroAtEnd {
		//fmt.Print("Yes Zero")
		return allSumList[:maxLength]
	} else {
		//fmt.Print("No Zero")
		return allSumList
	}
}

func main() {
	var inputA, inputB string
	var p int

	fmt.Scanln(&inputA)
	fmt.Scanln(&inputB)
	fmt.Scanln(&p)

	a := stringToIntArray(inputA)
	b := stringToIntArray(inputB)

	/*
		a := []int32{32150, 75958, 65273}
		b := []int32{89560, 79598}
		p = 91410
	*/

	sum := add(a, b, p)
	fmt.Println(sum)
}

func stringToIntArray(input string) []int32 {
	runes := []rune(input)
	array := make([]int32, len(runes))
	for i, r := range runes {
		num, _ := strconv.Atoi(string(r))
		array[len(runes)-1-i] = int32(num)
	}
	return array
}
