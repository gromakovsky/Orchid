def factorial(n : int64) → int64:
    if n < 2:
        return 1

    return n * factorial(n - 1)

def main():
    stdWriteInt(factorial(5000000))
    return

