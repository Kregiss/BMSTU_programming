package main

import (
	"fmt"
	"math"
)

type Rational struct {
	numerator   int // верх
	denominator int // низ
}

func NewRational(num, den int) Rational {
	return Rational{num, den}
}

func (r1 Rational) Add(r2 Rational) Rational {
	num := r1.numerator*r2.denominator + r2.numerator*r1.denominator
	den := r1.denominator * r2.denominator
	gcd := GCD(num, den)
	if gcd == 0 {
		//fmt.Print("Add gcd 0")
		return Rational{}
	}
	num /= gcd
	den /= gcd
	return NewRational(num, den)
}

func (r1 Rational) Sub(r2 Rational) Rational {
	if r1.denominator == 0 || r2.denominator == 0 {
		//fmt.Print("Sub 0")
		return Rational{}
	}
	num := r1.numerator*r2.denominator - r2.numerator*r1.denominator
	den := r1.denominator * r2.denominator
	gcd := GCD(num, den)
	if gcd == 0 {
		//fmt.Print("Sub gcd 0")
		return Rational{}
	}
	num /= gcd
	den /= gcd
	return NewRational(num, den)
}

func (r1 Rational) Mul(r2 Rational) Rational {
	if r1.denominator == 0 || r2.denominator == 0 {
		//fmt.Print("Mul 0")
		return Rational{}
	}
	num := r1.numerator * r2.numerator
	den := r1.denominator * r2.denominator
	gcd := GCD(num, den)
	if gcd == 0 {
		//fmt.Print("Mul gcd 0")
		return Rational{}
	}
	num /= gcd
	den /= gcd
	return NewRational(num, den)
}

func (r1 Rational) Div(r2 Rational) Rational {
	num := r1.numerator * r2.denominator
	den := r1.denominator * r2.numerator
	if den == 0 {
		//fmt.Print("Div 0 den")
		return Rational{}
	}
	gcd := GCD(num, den)
	if gcd == 0 {
		//fmt.Print("Div gcd 0")
		return Rational{}
	}
	num /= gcd
	den /= gcd
	return NewRational(num, den)
}

func GCD(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func (r Rational) Normalize() Rational {
	gcd := GCD(int(math.Abs(float64(r.numerator))), int(math.Abs(float64(r.denominator))))
	if gcd == 0 {
		//fmt.Print("Norm gcd 0")
		return Rational{}
	}
	sign := -1
	if r.denominator < 0 { // низ < 0
		if r.numerator < 0 { // верх < 0
			return NewRational(sign*r.numerator/gcd, sign*r.denominator/gcd)
		} else { // верх > 0
			return NewRational(sign*r.numerator/gcd, sign*r.denominator/gcd)
		}
	} else {
		return NewRational(r.numerator/gcd, r.denominator/gcd)
	}
}

func methodGaussa(array [][]int) [][]Rational {
	var row int = len(array)
	var col int = len(array[0])

	rationalMatrix := make([][]Rational, row)
	for i := range rationalMatrix {
		rationalMatrix[i] = make([]Rational, col)
		for j := range rationalMatrix[i] {
			rationalMatrix[i][j] = NewRational(array[i][j], 1)
		}
	}

	// ошибки
	for j := 0; j < col - 1; j++ {
		allZero := true
		for i := 0; i < row; i++ {
			if rationalMatrix[i][j].numerator != 0 {
				allZero = false
				break
			}
		}
		if allZero {
			//fmt.Print("allZero")
			return nil			
		}
	}

	for i := 0; i < row; i++ {
		for j := i + 1; j < row; j++ {
			ratio := rationalMatrix[j][i].Div(rationalMatrix[i][i])
			for k := i; k < col; k++ {
				rationalMatrix[j][k] = rationalMatrix[j][k].Sub(rationalMatrix[i][k].Mul(ratio))
			}
		}
	}

	/*
	// вывод матрицы
	for i := 0; i < row; i++ {
		for j := 0; j < col; j++ {
			fmt.Print(rationalMatrix[i][j], "\t")
		}
		fmt.Print("\n")
	}
	*/

	// ошибки
	hasSolution := true
	for i := 0; i < row; i++ {
		allZero := true
		for j := 0; j < col-1; j++ {
			if rationalMatrix[i][j].numerator != 0 {
				allZero = false
				break
			}
		}
		if allZero && rationalMatrix[i][col-1].numerator != 0 {
			hasSolution = false
			//fmt.Print("allZero---2")
			break
		}
	}

	if !hasSolution {
		return nil
	}
	

	result := make([][]Rational, row)
	for i := row - 1; i >= 0; i-- {
		result[i] = make([]Rational, 1)
		result[i][0] = rationalMatrix[i][col-1]
		for j := i + 1; j < row; j++ {
			result[i][0] = result[i][0].Sub(rationalMatrix[i][j].Mul(result[j][0]))
		}
		result[i][0] = result[i][0].Div(rationalMatrix[i][i])
		if (result[i][0] == Rational{}) {
			return nil
		}
	}
	/*
		fmt.Print("Обратно сделано\n")

		for i := 0; i < row; i++ {
			fmt.Print(result[i][0], "\t")
			fmt.Print("\n")
		}
	*/
	for i := range result {
		//fmt.Print("---------", i, "--------\n")
		//fmt.Print(result[i][0], result[0][0], "\n")
		result[i][0] = result[i][0].Normalize()
		//fmt.Print(result[i][0])
		if (result[i][0] == Rational{}) {
			return nil
		}
		
	}
	//fmt.Print("Вывод сделан\n")

	return result

}

func main() {
	var n int
	fmt.Scan(&n)

	array := make([][]int, n)
	for i := range array {
		array[i] = make([]int, n+1)
	}

	// n строк (i), n + 1 столбцов (j)
	for i := 0; i < n; i++ {
		for j := 0; j < n+1; j++ {
			var x int
			fmt.Scan(&x)
			array[i][j] = x
		}
	}

	finishMatrix := methodGaussa(array)

	if finishMatrix == nil {
		fmt.Println("No solution")
	} else {
		//fmt.Print("Итоговая матрица: \n")
		for _, row := range finishMatrix {
			fmt.Printf("%d/%d\n", row[0].numerator, row[0].denominator)
		}
	}
}

/*
3
-4 -1 8 2
7 -7 7 3
5 -1 -4 7

Answer:
377/21
214/7
274/21


4
-1  -8   1   1   2
-8   4  -3  -1  -7
-6  -2  -4  -2  -7
 3  -6  -1  -2  -6

Answer:
111/73
-5/146
-312/73
549/73

1
0  -6

Answer:
No solution

4
 6   5   6   0  -2
 6   3   6   5  -9
-3   0   5  -9  -7
 0  -2   0   5   3

Answer:
No solution

4
-3  -2   3   0  -5
-3   8   1   2   1
-9  -1  -8  -7   0
-5  -9   6   1   8

Answer:
-68/33
-49/22
-172/33
589/66
*/
