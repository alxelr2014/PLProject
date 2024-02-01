z = [0 , 1];
print(z);
t = [1 + 2, 4 ** 2];
print(t);
print(z + t);
print([t]);
u = z + t;
print(u[3] - 1);
print([0,1,2][1]);

def arrmult(s = [0],k = 2):
    if k == 1:
        return s;
    else:
        return arrmult(s,k-1) + s;;;

print(arrmult(u + [True], 3));
