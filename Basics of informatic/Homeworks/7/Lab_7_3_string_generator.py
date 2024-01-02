# Lab_7_3_string_generator.py

import random

def print_string(len_str, k_str):

    symbols = "\"qwertyuiopasdfghjklzxcvbnm,./|1234567890;'-=()_+"\
              "QWERTYUIOPASDFGHJKL:ZXCVBNM{}[]<>?!@#$%^&*"
    
    for i in range(k_str):
        finish_str = ''

        for j in range(len_str):
            finish_str += symbols[random.randint(0, len(symbols) - 1)]
        
        print(finish_str)