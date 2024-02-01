for i in [2,3,4 - 1,5 - 3]:
    print(i);
    if i == 2:
        continue;
    else:
        print(-1);;
    if i == 4:
        break;
    else:
        print(0);;
    print(- i);;

def func(x = 9):
    if x == 0:
        return [0];
    else:
        return func(x-1) + [x];;;

for i in func():
    print(i);;