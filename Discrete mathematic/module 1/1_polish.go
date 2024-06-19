package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func isDelimiter(c rune) bool {
	return c == ' '
}

func isOperator(c rune) bool {
	operators := "+-*()"
	for _, op := range operators {
		if byte(op) == byte(c) {
			return true
		}
	}
	return false
}

func getPriority(s rune) byte {
	switch s {
	case '(':
		return 0
	case ')':
		return 1
	case '+', '-':
		return 2
	case '*':
		return 4
	default:
		return 5
	}
}

func getPrefixExpression(input string) string {
	output := ""
	operStack := []rune{}

	for i := len(input) - 1; i >= 0; i-- {
		if isDelimiter(rune(input[i])) {
			continue
		}

		if input[i] >= '0' && input[i] <= '9' {
			num := ""
			for !isDelimiter(rune(input[i])){
            
				num = string(input[i]) + num
				i--
				if i < 0 {
					break
				}
			}
			output += num + " "
			i++
		}

		if isOperator(rune(input[i])) {
			if input[i] == ')' {
				operStack = append(operStack, rune(input[i]))
			} else if input[i] == '(' {
				for operStack[len(operStack) - 1] != ')' {
					output += string(operStack[len(operStack) - 1]) + " "
					operStack = operStack[:len(operStack) - 1]
				}
				operStack = operStack[:len(operStack) - 1] // Удаляем (
			} else {
				for len(operStack) > 0 && getPriority(rune(input[i])) < getPriority(rune(operStack[len(operStack) - 1])) {
					output += string(operStack[len(operStack) - 1]) + " "
					operStack = operStack[:len(operStack) - 1]
				}
				operStack = append(operStack, rune(input[i]))
			}
		}
	}

	for len(operStack) > 0 {
		output += string(operStack[len(operStack) - 1]) + " "
		operStack = operStack[:len(operStack) - 1]
	}

	return reverseString(output)
}

func countPrefixExpression(input string) float64 {
	temp := []float64{}

	for i := len(input) - 1; i >= 0; i-- {
		if input[i] >= '0' && input[i] <= '9' {
			num := ""
			for !isDelimiter(rune(input[i])) && !isOperator(rune(input[i])) {
				num = string(input[i]) + num
				i--
				if i < 0 {
					break
				}
			}
			val, _ := strconv.ParseFloat(num, 64)
			temp = append(temp, val)
			i++
		} else if isOperator(rune(input[i])) {
			a := temp[len(temp) - 1]
			b := temp[len(temp) - 2]
			temp = temp[:len(temp) - 2]

			var result float64
			switch input[i] {
			case '+':
				result = a + b
			case '-':
				result = a - b
			case '*':
                if (a == 0 || b == 0) {
                    result = 0
                } else {
				    result = a * b
                }
			}
			temp = append(temp, result)
		}
	}

	return temp[0]
}

func reverseString(str string) string {
	runes := []rune(str)
	for i, j := 0, len(runes)-1; i < j; i, j = i + 1, j - 1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func addSpacesBetweenRunes(input string) string {
	output := " "
	for i, char := range input {
		if i > 0 && input[i-1] != ' ' { 
			output += " " 
		}
		output += string(char)
	}
	return output
}

func main4() {
	reader := bufio.NewReader(os.Stdin)
    
    input, _ := reader.ReadString('\n')
    input = input[:len(input) - 1]

    inputWithSpace := addSpacesBetweenRunes(input)
    //fmt.Print(inputWithSpace + "\n")
	prefixExpression := getPrefixExpression(inputWithSpace)
	result := countPrefixExpression(prefixExpression)

	//fmt.Println("Выражение без скобок:", prefixExpression)
	fmt.Println(result)
}

/*
(* 5 (+ 3 4))



https://habr.com/ru/sandbox/56187/
*/
