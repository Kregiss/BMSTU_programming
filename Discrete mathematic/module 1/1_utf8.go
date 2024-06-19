package main

import (
	"fmt"
	"strconv"
)

func encode(utf32 []rune) []byte {
	var utf8Bytes []byte
	for _, r := range utf32 {
		if r <= 0x7F {
			utf8Bytes = append(utf8Bytes, byte(r))
		} else if r <= 0x7FF {
			utf8Bytes = append(utf8Bytes, byte(0xC0|(r>>6)))
			utf8Bytes = append(utf8Bytes, byte(0x80|(r&0x3F)))
		} else if r <= 0xFFFF {
			utf8Bytes = append(utf8Bytes, byte(0xE0|(r>>12)))
			utf8Bytes = append(utf8Bytes, byte(0x80|((r>>6)&0x3F)))
			utf8Bytes = append(utf8Bytes, byte(0x80|(r&0x3F)))
		} else if r <= 0x10FFFF {
			utf8Bytes = append(utf8Bytes, byte(0xF0|(r>>18)))
			utf8Bytes = append(utf8Bytes, byte(0x80|((r>>12)&0x3F)))
			utf8Bytes = append(utf8Bytes, byte(0x80|((r>>6)&0x3F)))
			utf8Bytes = append(utf8Bytes, byte(0x80|(r&0x3F)))
		}
	}
	return utf8Bytes
}

func decode(utf8 []byte) []rune {
	var utf32Runes []rune
	for i := 0; i < len(utf8); {
		if utf8[i]&0x80 == 0 {
			utf32Runes = append(utf32Runes, rune(utf8[i]))
			i++
		} else if utf8[i]&0xE0 == 0xC0 {
			utf32Runes = append(utf32Runes, rune(utf8[i]&0x1F)<<6|rune(utf8[i+1]&0x3F))
			i += 2
		} else if utf8[i]&0xF0 == 0xE0 {
			utf32Runes = append(utf32Runes, rune(utf8[i]&0x0F)<<12|rune(utf8[i+1]&0x3F)<<6|rune(utf8[i+2]&0x3F))
			i += 3
		} else if utf8[i]&0xF8 == 0xF0 {
			utf32Runes = append(utf32Runes, rune(utf8[i]&0x07)<<18|rune(utf8[i+1]&0x3F)<<12|rune(utf8[i+2]&0x3F)<<6|rune(utf8[i+3]&0x3F))
			i += 4
		} 
	}
	return utf32Runes
}

func main1() {
	//utf32Text := []rune{'A', 'é', '€'} // "A", "é", "€"
	/*
	fmt.Println("UTF-32:", utf32Text)

	utf8Encoded := encode(utf32Text)
	fmt.Println("UTF-8 encoded:", utf8Encoded)

	utf32Decoded := decode(utf8Encoded)
	fmt.Println("UTF-32 decoded:", utf32Decoded)
	*/

	var input string
	fmt.Scanln(&input)

	var utf32Text []rune
	for _, s := range input {
		code, _ := strconv.ParseUint(string(s), 16, 32)
		utf32Text = append(utf32Text, rune(code))
	}

	fmt.Println(encode(utf32Text))
	fmt.Println(decode(encode(utf32Text)))
}
