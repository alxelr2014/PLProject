global hi;
hi = 2;
hi = hi + 3;
def func(x = 3):
    eff = True;
    if eff:
        hi = x - 3;
    else:
        print(-100);;
    return hi;;
t = func(2);
z = [2] + [hi];
print(hi, func(4),hi);

