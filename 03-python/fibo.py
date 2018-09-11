# Fibonacci numbers module

def fib(n):
    """
    Write Fibonacci series up to n.
    """
    a, b = 0, 1
    while b < n:
        print(b, end=' ')
        a, b = b, a+b
    print()

def get5():
    return 5