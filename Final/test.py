

# ### IMPORTANT NOTE ###
# ### This is the only test for which the expected output
# ### is *not* the same as the one obtained with Python.
# ### The difference comes from arithmetic: Mini Python uses
# ### machine arithmetic for division and module, whereas Python uses
# ### something else (where the modulus has the sign of the *second* operand).

# # fixed-point arithmetic
# # precision q = 8192 i.e. 13 bits for the decimal part

# def add(x, y):
#     return x + y
# def sub(x, y):
#     return x - y
# def mul(x, y):
#     t = x * y
#     return (t + 8192 // 2) // 8192
# def div(x, y):
#     t = x * 8192
#     return (t + y // 2) // y
# def of_int(x):
#     return x * 8192

# def iter(n, a, b, xn, yn):
#     if n == 100: return 1
#     xn2 = mul(xn, xn)
#     yn2 = mul(yn, yn)
#     if add(xn2, yn2) > of_int(4): return 0
#     return iter(n+1, a, b, add(sub(xn2, yn2), a), add(mul(of_int(2), mul(xn, yn)), b))

# def inside(x, y):
#     return iter(0, x, y, of_int(0), of_int(0))

# def main():
#     xmin = of_int(-2)
#     xmax = of_int(1)
#     steps = 40
#     deltax = div(sub(xmax, xmin), of_int(2 * steps))
#     ymin = of_int(-1)
#     ymax = of_int(1)
#     deltay = div(sub(ymax, ymin), of_int(steps))
#     for i in list(range(steps)):
#         y = add(ymin, mul(of_int(i), deltay))
#         s = ""
#         for j in list(range(2 * steps)):
#             x = add(xmin, mul(of_int(j), deltax))
#             if inside(x, y): s = s + "0"
#             else: s = s + "1"
#         print(s)

# main()

# N-queens

def abs(x):
    if x < 0: return -x
    else: return x

def check(b, i, n):
    for j in list(range(i)):
        if b[i] == b[j] or abs(b[i]-b[j]) == abs(i-j):
            return False
    return True

def count(b, i, n, l):
    if i == n: return 1
    c = 0
    for x in list(range(n)):
        # print ("for x in list(range(n)): b, i, x, n, c")
        # print(b)
        # print(i)
        # print(x)
        # print(n)
        # print(c)
        b[i] = x
        if check(b, i, n):
            # print ("if check: b, i, x, n, c")
            # print(b)
            # print(i)
            # print(x)
            # print(n)
            # print(c)
            # l = l + [[i, n, c]]
            temp = count(b, i+1, n, l)
            # print(l)
            c = c + temp
    return c

def q(n):
    return count(list(range(n)), 0, n, [])

# for n in list(range(10)):
#     # print("ans:")
#     print(q(n))
print(q(9))
