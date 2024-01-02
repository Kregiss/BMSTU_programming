#!/usr/bin/env python3
def memo(f):
    a = {}

    def wrapper(*args):
        if args not in a:
            a[args] = f(*args)
        return a[args]

    return wrapper

#@memo
def factorial(n):
    if n == 1:
        return 1
    return factorial(n - 1) * n

factorial = memo(factorial)


#print(factorial(300))