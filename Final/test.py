def f(i, j):
    if i >= j:
        return []
    x = f(i + 1, j)
    print(x)
    return [i] + x

print(f(4, 7))
