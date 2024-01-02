#!/usr/bin/python3
import sys
from Lab_7_3_string_generator import print_string

def main():
    
    if len(sys.argv) == 3:
        try:
            len_str = int(sys.argv[1])
            k_str = int(sys.argv[2])
        except ValueError as e:
            print("Аргументы должны быть целыми числами.")
            sys.exit(1)
    else:
        len_str = int(input("Введите длину строки: "))
        k_str = int(input("Введите количество строк: "))

    print_string(len_str, k_str)

if __name__ == '__main__':
    main()