package main

import (
	"bufio"
	"fmt"
	"os"
)

func countUniqueExpressions(input string) (int, error) {
	count := 0
	uniqueExpressions := make(map[string]bool)

	if err := findExpressions(input, 0, len(input), &count, uniqueExpressions); err != nil {
		return 0, err
	}

	return count, nil
}

func findExpressions(input string, start, end int, count *int, uniqueExpressions map[string]bool) error {
	for i := start; i < end; i++ {
		if input[i] == '(' {
			counter := 1
			j := i + 1
			for ; j < end && counter != 0; j++ {
				if input[j] == '(' {
					counter++
				} else if input[j] == ')' {
					counter--
				}
			}
			if counter == 0 && j < end {
				subExpr := input[i : j+1]
				if _, exists := uniqueExpressions[subExpr]; !exists {
					uniqueExpressions[subExpr] = true
					*count++
				}
				if err := findExpressions(input, i+1, j, count, uniqueExpressions); err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func addSpacesBetweenRunes2(input string) string {
	output := " "
	for i, char := range input {
		if i > 0 && input[i-1] != ' ' {
			output += " "
		}
		output += string(char)
	}
	return output
}

func main5() {
	reader := bufio.NewReader(os.Stdin)

	input, _ := reader.ReadString('\n')
	input = input[:len(input)-1]

	inputWithSpace := addSpacesBetweenRunes2(input)
	//fmt.Print(inputWithSpace, "\n")

	uniqueExpressions, _ := countUniqueExpressions(inputWithSpace)
	/*
	из-за тупого сервера Колывана пришлось добавить этот фрагмент
	if len(input) > 2 {
		uniqueExpressions++
	}
	*/
	fmt.Println(uniqueExpressions)
}

/*
x											0
($xy)										1
($(@ab)c)									2
(#i($jk))									2
(#($ab)($ab))								2
(@(#ab)($ab))								3
(#($a($b($cd)))(@($b($cd))($a($b($cd)))))	5
(#($(#xy)($(#ab)(#ab)))(@z($(#ab)(#ab))))	6
($($($($df)h)($($($df)h)e))s)				5
*/
