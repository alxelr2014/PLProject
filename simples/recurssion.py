def f(x = 8):
    if x == 0:
        return [0];
    else:
        return f(x-1) + [x];;;

print(f(10));

def fib(x = 3):
    if x == 0:
        return 1;
    else:
        if x == 1:
            return 1;
        else:
            return fib(x - 1) + fib(x- 2);;;;
for i in f():
    print(fib(i));;
