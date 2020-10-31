def fact(x):
    cur = 1
    res = 1
    while cur <= x:
        res = res * cur
        cur = cur + 1
    return res

print(fact(1))
print(fact(3))
print(fact(5))
